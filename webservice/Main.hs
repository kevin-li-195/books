{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Reader
import Control.Monad.Except

import Data.Aeson
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

bookRenewalT :: Config -> BookRenewalMonad :~> Handler
bookRenewalT c = NT $ \ma -> do
  (liftIO $ flip runReaderT c $ runExceptT $ runRenewal ma) >>= \case
    Right a -> pure a
    Left err -> throwError $ toServantErr err

toServantErr :: RenewalError -> ServantErr
toServantErr (Unknown s) 
  = err500 
  { errBody = LBS.fromStrict $ T.encodeUtf8 $ T.pack s }
toServantErr (NoSuchUser u) = err404 { errBody = encode u }

app :: Config -> Application
app c = serve api (enter (bookRenewalT c) renewalServer) where
  api :: Proxy RenewalApi
  api = Proxy
