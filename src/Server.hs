{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Monad.Trans

import qualified Data.ByteString as B
import qualified Data.Text as T

import Data.Time.Clock

import Database.PostgreSQL.Simple
import Web.Stripe.Charge
import Web.Stripe

import Servant
import Servant.API
import Server.Types

import System.Process
import System.Exit

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.String

libraryScraper :: String
libraryScraper = "./library-scraper"

renewalServer :: Config -> Server RenewalApi
renewalServer cfg = register cfg :<|> pay cfg :<|> echo where
  echo :: B.ByteString -> Handler B.ByteString
  echo = pure

registerSQLInsertQuery :: Query
registerSQLInsertQuery = "insert into profile (username, password, email_address, phone_number) values (?, ?, ?, ?) returning id"

notificationSettingInsertQuery :: Query
notificationSettingInsertQuery = "insert into notification_setting (profile_id, notification_level) values (?, ?)"

selectUserPassQuery :: Query
selectUserPassQuery = "select password from profile where username = ?"

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
    <$> tdCell
    <*> tdCell
    <*> tdCell
    <*> tdCell
    <*> tdCell
    <*> tdCell
    <*> tdCell
    <*> tdCell

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
          <*> tdCell
          <*> tdCell
          <*> tdCell
          <*> tdCell
          <*> tdCell
          <*> tdCell
          <*> tdCell
          <*> tdCell

-- | Register the 'Registrant' in the database,
-- and return the freshly generated detailed profile.
-- from immediately scraping the library webpage.
register :: Config -> Registrant -> Handler DetailedProfile
register Config{..} Registrant{..} = do
  [Only id] :: [Only Int] <- liftIO $ query dbconn registerSQLInsertQuery
            (unUsername registrantUsername, pass, notificationEmail, phoneNumber)
  liftIO $ execute dbconn notificationSettingInsertQuery (id, triggerToInt trigger)
  (exitcode, stdout, stderr) <- liftIO $
            readProcessWithExitCode
                libraryScraper
                [ "lookup"
                , T.unpack $ unUsername registrantUsername
                , T.unpack pass
                ]
                ""
  case exitcode of
    ExitSuccess -> case runParser detailedBooks "lookup" stdout of
            Right a -> pure $ DetailedProfile a
            Left b -> fail $ "Parser error: " ++ (show b) ++ "Dumping stdout: " ++ stdout ++ "Dumping stderr: " ++ stderr
    ExitFailure _ -> fail $ "Detailed book scrape failure: "
                ++ stdout
                ++ stderr

-- | Base 1 CAD payment.
pmt :: StripeRequest CreateCharge
pmt = createCharge (Amount 100) CAD

pay :: Config -> PaymentInfo -> Handler RenewalProfile
pay conf@Config{..} PaymentInfo{..} = do
  result <- liftIO $ stripe stripeConfig $ pmt -&- tokenId
  case result of
    Right details
      -> if chargePaid details
         then liftIO $ do
            validateAccount conf paymentUsername
            renew conf paymentUsername
            getRenewalProfile conf paymentUsername
         else fail $ "Charge was not paid." ++ (show details)
    Left err -> fail $ show err

validationQuery :: Query
validationQuery = "update profile set (paid, time_paid) = (true, now()) where username = ?"

-- | Validates the given 'Username' to mark it as paid.
validateAccount :: Config -> Username -> IO ()
validateAccount Config{..} u
  = execute dbconn validationQuery (Only $ unUsername u) >> pure ()

getRenewalProfileQuery :: Query
getRenewalProfileQuery = "select (i.description, i.item_status, i.due_date, r.created_at) from profile p inner join renewal r on (r.profile_id = p.id) inner join renewal_item i on (i.renewal_id = r.id) where p.username = ? and r.created_at = (select max(r.created_at) from renewal r inner join profile p on (p.id = r.profile_id) where p.username = ?)"

-- | Return the current 'RenewalProfile' of the given 'Username'
-- based on the current database status.
getRenewalProfile :: Config -> Username -> IO RenewalProfile
getRenewalProfile Config{..} u = do
  rows :: [(T.Text, T.Text, UTCTime, UTCTime)] <-
    query dbconn getRenewalProfileQuery (unUsername u, unUsername u)
  pure $ RenewalProfile $
          ( \(desc, stat, due, last) -> DBBook desc stat due last )
          <$> rows

retrievePassQuery :: Query
retrievePassQuery = "select password from profile where username = ?"

-- | Try to renew all books of given 'Username' and
-- return results.
renew :: Config -> Username -> IO [RenewalResult]
renew Config{..} u = do
  [Only pass] <- query dbconn retrievePassQuery (Only $ unUsername u)
  (exitcode, stdout, stderr) <- liftIO $
            readProcessWithExitCode
                libraryScraper
                [ "renew"
                , T.unpack $ unUsername u
                , T.unpack pass
                ]
                ""
  case exitcode of
    ExitSuccess -> case runParser renewalResultsParser "renewal" stdout of
      Right a -> pure a
      Left b -> fail $ "Parser error: " ++ (show b) ++ "Dumping stdout: " ++ stdout ++ "Dumping stderr: " ++ stderr
    ExitFailure _ -> fail $ "Renewal failure occurred. \
      \Dumping stdout and stderr: "
      ++ stdout
      ++ stderr
