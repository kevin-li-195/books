module Renewal.LibraryScraper where

import Renewal.Types

import qualified Data.Text as T
import System.Process
import System.Exit ( ExitCode(..) )
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.String

data LibraryScraperError
  = InvalidCredentials
  | ScraperParseError (ParseError (Token String) Dec)
  | ScraperExitCode Int
  deriving (Eq, Show)

type LibraryScraperInfo
  = (String, String, LibraryScraperError)

type LibraryScraper a = IO (Either LibraryScraperInfo a)

formatError :: LibraryScraperInfo -> String
formatError (out, err, e)
  = show e ++ "\nstdout: " ++ out ++ "\nstderr: " ++ err

libraryScraper :: String
libraryScraper = "./library-scraper"

runLibraryScraper :: [String] -> IO (ExitCode, String, String)
runLibraryScraper args = readProcessWithExitCode libraryScraper args ""

checkUser :: Username -> Password -> LibraryScraper [DetailedBook]
checkUser (Username u) (Password p) = do
  (code, out, err) <- runLibraryScraper ["lookup", T.unpack u, T.unpack p]
  case code of
    ExitSuccess -> case runParser detailedBooks "lookup" out of
      Right a -> pure (Right a)
      Left b -> pure (Left (out, err, ScraperParseError b))
    ExitFailure n -> pure (Left (out, err, ScraperExitCode n))

renew :: Username -> Password -> LibraryScraper [RenewalResult]
renew (Username u) (Password p) = do
  (code, out, err) <- runLibraryScraper ["renew", T.unpack u, T.unpack p]
  case code of
    ExitSuccess -> case runParser renewalResultsParser "renewal" out of
      Right a -> pure (Right a)
      Left b -> pure (Left (out, err, ScraperParseError b))
    ExitFailure n -> pure (Left (out, err, ScraperExitCode n))

-- | Parser for detailedBooks
-- given stdout output.
detailedBooks :: Parser [DetailedBook]
detailedBooks = header *> ((between sc sc detailedBook) `manyTill` eof)

-- | Parser for list of renewal results
renewalResultsParser :: Parser [RenewalResult]
renewalResultsParser
  = header *> sc *> ((between sc sc renewalResult) `manyTill` eof)

renewalResult :: Parser RenewalResult
renewalResult = between tr tr $ do
  tdCell
  RenewalResult
    <$> (Description <$> tdCell)
    <*> (ItemStatus <$> tdCell)
    <*> tdCell
    <*> tdCell
    <*> tdCell
    <*> tdCell
    <*> tdCell
    <*> (Comment <$> tdCell)

-- | Space consuming parser.
sc :: Parser ()
sc = skipMany spaceChar

-- | Table data tag
td :: Parser String
td = try (string "<td>") <|> (string "</td>")

-- | Table row tag
tr :: Parser String
tr = try (string "<tr>") <|> (string "</tr>")

-- | Table header tag
th :: Parser String
th = try (string "<th>") <|> (string "</th>")

-- | Parser for text.
text :: Parser String
text = many anyChar

thCell :: Parser String
thCell = between sc sc $ th *> (anyChar `manyTill` th)

-- | Content in the header.
headerElems :: Parser [String]
headerElems = thCell `sepBy` sc

-- | The entire header
header :: Parser [String]
header = between tr tr headerElems

-- | The whole row.
detailedBook :: Parser DetailedBook
detailedBook = between tr tr dataRow

-- | Data cell
tdCell :: Parser T.Text
tdCell = T.pack <$> (between sc sc $ td *> (anyChar `manyTill` td))

-- | The data contents of a single table row.
-- Table row has length 11, but
-- 'DetailedBook' has 9 fields because
-- we skip the first few.
dataRow :: Parser DetailedBook
dataRow = between sc sc $ do
      tdCell
      tdCell
      DetailedBook
          <$> tdCell
          <*> (Description <$> tdCell)
          <*> tdCell
          <*> tdCell
          <*> tdCell
          <*> tdCell
          <*> tdCell
          <*> tdCell
          <*> tdCell

