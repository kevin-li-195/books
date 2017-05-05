{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Reader
import Control.Monad.Except

import Control.Concurrent.Async
import Control.Exception ( throwIO )
import Data.Aeson
import Data.Pool
import qualified Data.ByteString.Lazy as LBS
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import Network.Mail.Mime ( Address(..) )
import Network.Mail.SMTP ( sendMail, simpleMail, plainTextPart )
import Network.Wai.Handler.Warp ( run )
import Network.Wai ( Application )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Servant hiding ( NoSuchUser )
import System.Environment ( getEnv )
import System.Directory ( getCurrentDirectory )

import Database.PostgreSQL.Simple.Transaction

import Server
import Renewal.Types
import Renewal.Monad

defaultPayment :: Int
defaultPayment = 100

service :: NominalDiffTime
service = fromInteger $ 86400 * 120

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  putStrLn $ "working in " ++ cwd
  connstr <- T.encodeUtf8 . T.pack <$> getEnv "BOOKS_PSQL"
  key <- T.encodeUtf8 . T.pack <$> getEnv "BOOKS_STRIPE_KEY_SECRET"
  config <- newConfig connstr key defaultPayment service
  efrom <- addr . T.pack <$> getEnv "BOOKS_EMAIL_FROM"
  eto <- map (addr . T.pack) . read <$> getEnv "BOOKS_EMAIL_TO"
  let emailConfig = EmailConfig { emailFrom = efrom, emailTo = eto }
  run 8084 (logStdoutDev (app emailConfig config))

bookRenewalT :: EmailConfig -> RequestConfig -> BookRenewalMonad :~> Handler
bookRenewalT emailConf RequestConfig{..} = NT $ \ma ->
  withResource reqDbconn $ \conn -> do
    let sconf = ServerConfig
              { serverStripeConfig = reqStripeConfig
              , serverDbconn = conn
              , serverPayment = reqPayment
              , serverServiceTime = reqServiceTime
              }

    let io = flip runReaderT sconf (runExceptT (runRenewal ma))
    let transacted = withTransaction conn (emailBracket emailConf io)
    liftIO transacted >>= \case
        Right a -> pure a
        Left err -> throwError $ toServantErr err

emailBracket :: EmailConfig -> IO a -> IO a
emailBracket EmailConfig{..} m = do
  async m >>= waitCatch >>= \case
    Left e -> do
      sendMail "localhost" $
        simpleMail
          emailFrom
          emailTo
          []
          []
          "500 internal server error"
          [ plainTextPart . TL.pack $
            "A fatal error has occurred.\n\n" ++ show e
          ]
      throwIO e
    Right x -> pure x

data EmailConfig
  = EmailConfig
    { emailFrom :: Address
    , emailTo :: [Address]
    }

toServantErr :: RenewalError -> ServantErr
toServantErr (Unknown s)
  = err500
  { errBody = LBS.fromStrict $ T.encodeUtf8 $ T.pack s }
toServantErr (NoSuchUser u) = err404 { errBody = encode u }

app :: EmailConfig -> RequestConfig -> Application
app e c = serve api (enter (bookRenewalT e c) renewalServer) where
  api :: Proxy RenewalApi
  api = Proxy

addr :: Text -> Address
addr email = Address { addressName = Nothing, addressEmail = email }
