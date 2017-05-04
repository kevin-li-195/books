module Main where

import Renewal.Types

import Data.ByteString ( ByteString )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.PostgreSQL.Simple
import System.Environment ( getEnv )

main :: IO ()
main = do
  conn <- connectPostgreSQL =<< (utf8 <$> getEnv "BOOKS_PSQL")
  pure ()

utf8 :: String -> ByteString
utf8 = T.encodeUtf8 . T.pack
