{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Reader
import Control.Monad.Except

import Data.Aeson
import Data.Pool
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
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
  run 8084 (logStdoutDev (app config))

bookRenewalT :: RequestConfig -> BookRenewalMonad :~> Handler
bookRenewalT RequestConfig{..} = NT $ \ma ->
  withResource reqDbconn $ \conn -> do
    let sconf = ServerConfig
              { serverStripeConfig = reqStripeConfig
              , serverDbconn = conn
              , serverPayment = reqPayment
              , serverServiceTime = reqServiceTime
              }
    (liftIO 
      $ withTransaction conn 
      $ flip runReaderT sconf 
      $ runExceptT 
      $ runRenewal ma) >>= \case
        Right a -> pure a
        Left err -> throwError $ toServantErr err

toServantErr :: RenewalError -> ServantErr
toServantErr (Unknown s) 
  = err500 
  { errBody = LBS.fromStrict $ T.encodeUtf8 $ T.pack s }
toServantErr (NoSuchUser u) = err404 { errBody = encode u }

app :: RequestConfig -> Application
app c = serve api (enter (bookRenewalT c) renewalServer) where
  api :: Proxy RenewalApi
  api = Proxy
