{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Renewal.DB as DB
import qualified Renewal.LibraryScraper as Library
import Renewal.Types

import Data.ByteString ( ByteString )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.PostgreSQL.Simple
import System.Environment ( getEnv )
import System.IO ( hPutStrLn, stderr )

main :: IO ()
main = do
  conn <- connectPostgreSQL =<< (utf8 <$> getEnv "BOOKS_PSQL")
  DB.getAllActiveUsers conn >>= mapM_ (processUser conn)

processUser :: Connection -> DB.DBProfile -> IO ()
processUser conn DB.DBProfile{..} = do
  hPutStrLn stderr $
    "renewing books for " ++ T.unpack (unUsername dbProfileUsername)
  Library.renew dbProfileUsername dbProfilePassword >>= \case
    Right results -> do
      renewalId <- DB.createRenewal dbProfileUsername conn
      _ <- DB.createRenewalItems renewalId results conn
      pure ()
    Left e -> fail (Library.formatError e)

utf8 :: String -> ByteString
utf8 = T.encodeUtf8 . T.pack
