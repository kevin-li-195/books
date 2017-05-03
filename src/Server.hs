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
libraryScraper = "library-scraper"

renewalServer :: Config -> Server RenewalApi
renewalServer cfg = register cfg :<|> pay cfg :<|> echo where
  echo :: B.ByteString -> Handler B.ByteString
  echo = pure

registerSQLInsertQuery :: Query
registerSQLInsertQuery = "insert into profile (username, password, email_address, phone_number) values (?, ?, ?, ?)"

notificationSettingInsertQuery :: Query
notificationSettingInsertQuery = "insert into notification_setting (profile_id, notification_level) values (?, ?) returning id"

selectUserPassQuery :: Query
selectUserPassQuery = "select password from profile where username = ?"

-- | Parser for detailedBooks
-- given
detailedBooks :: Parser [DetailedBook]
detailedBooks = header *> many detailedBook

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

-- | Content in the header.
headerElems :: Parser [String]
headerElems = sepBy1 text (some th)

-- | The entire header
header :: Parser [String]
header = between tr tr headerElems

-- | The whole row.
detailedBook :: Parser DetailedBook
detailedBook = between tr tr dataRow

-- | Data cell
tdCell :: Parser String
tdCell = between td td text

-- | The data contents of a single table row.
-- Table row has length 11, but
-- 'DetailedBook' has 9 fields because
-- we skip the first few.
dataRow :: Parser DetailedBook
dataRow = do
  tdCell
  tdCell
  DetailedBook
    <$> (T.pack <$> tdCell)
    <*> (T.pack <$> tdCell)
    <*> (T.pack <$> tdCell)
    <*> (T.pack <$> tdCell)
    <*> (T.pack <$> tdCell)
    <*> (T.pack <$> tdCell)
    <*> (T.pack <$> tdCell)
    <*> (T.pack <$> tdCell)
    <*> (T.pack <$> tdCell)

-- | Register the 'Registrant' in the database,
-- and return the freshly generated detailed profile.
-- from immediately scraping the library webpage.
register :: Config -> Registrant -> Handler DetailedProfile
register Config{..} Registrant{..} = do
  id <- liftIO $ execute dbconn registerSQLInsertQuery
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
    ExitSuccess -> case runParser detailedBooks "" stdout of
            Right a -> pure $ DetailedProfile a
            Left b -> fail $ "Parser error: " ++ (show b)
    ExitFailure _ -> fail $ "Detailed book scrape failure: "
                ++ stdout
                ++ stderr

-- | Base 1 CAD payment.
pmt :: StripeRequest CreateCharge
pmt = createCharge (Amount 1) CAD

pay :: Config -> PaymentInfo -> Handler RenewalProfile
pay Config{..} PaymentInfo{..} = do
  result <- liftIO $ stripe stripeConfig $ pmt -&- tokenId
  case result of
    Right details
      -> if chargePaid details
         then liftIO $ do
            validateAccount paymentUsername
            renew paymentUsername
            getRenewalProfile paymentUsername
         else fail $ "Charge was not paid." ++ (show details)
    Left err -> fail $ show err

-- | Validates the given 'Username' to mark it as paid.
validateAccount u = error "unimplemented validateAccount"

-- | Return the current 'RenewalProfile' of the given 'Username'
-- based on the current database status.
getRenewalProfile :: Username -> IO RenewalProfile
getRenewalProfile user = error "unimplemented getRenewalProfile"

-- | Try to renew all books of given 'Username' and
-- return results.
renew :: Username -> IO RenewalResults
renew user = error "unimplemented renew"
