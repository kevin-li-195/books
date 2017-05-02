module Main where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Network.Wai.Handler.Warp ( run )
import Network.Wai ( Application )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Servant
import System.Environment ( getEnv )

import Server
import Server.Types

main :: IO ()
main = do
  connstr <- BS.pack <$> getEnv "BOOKS_PSQL"
  key <- T.pack <$> getEnv "BOOKS_STRIPE_KEY_SECRET"
  config <- newConfig connstr key
  run 8084 (logStdoutDev (app config))

app :: Config -> Application
app config = serve api (renewalServer config) where
  api :: Proxy RenewalApi
  api = Proxy
