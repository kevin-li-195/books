{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.Types where

import Data.Aeson
import qualified Data.ByteString as BS
import Database.PostgreSQL.Simple

import Data.Time.Clock
import qualified Data.Text as T

import GHC.Generics

import Servant.API

import Web.Stripe
import Web.Stripe.Charge

type RenewalApi
  = "register"
    :> ReqBody '[JSON] Registrant
    :> Post '[JSON] DetailedProfile
  :<|> "payment"
    :> ReqBody '[JSON] PaymentInfo
    :> Post '[JSON] RenewalProfile

data PaymentInfo
  = PaymentInfo
  { tokenId :: TokenId
  , paymentUsername :: Username
  }
  deriving (FromJSON, Generic)

-- | New type wrapper for usernames.
newtype Username = Username { unUsername :: T.Text }

instance FromJSON Username where
  parseJSON (String s) = pure (Username s)
  parseJSON _ = fail "cannot parse username from non-string"

instance ToJSON Username where
  toJSON (Username s) = String s

-- | Data provided by the registrant.
data Registrant
  = Registrant
  { registrantUsername :: Username
  -- ^ McGill email
  , notificationEmail :: T.Text
  -- ^ Optional notification email
  , pass :: T.Text
  -- ^ McGill email password
  , phoneNumber :: T.Text
  -- ^ Phone number for notifications
  , trigger :: Trigger
  -- ^ Trigger for notifications
  }
  deriving (FromJSON, ToJSON, Generic)

-- | Server config.
data Config
  = Config
  { stripeConfig :: StripeConfig
  , dbconn :: Connection
  }

-- | Connects to the DB with the given connection string and uses the given
-- bytestring as the stripe key.
newConfig :: BS.ByteString -> BS.ByteString -> IO Config
newConfig connstr key = do
  conn <- connectPostgreSQL connstr
  pure Config
    { dbconn = conn
    , stripeConfig = StripeConfig (StripeKey key)
    }

data RenewalResults
  = RenewalResults
  { renewalDescription :: T.Text
  , renewalItemStatus :: T.Text
  , renewalDueDate :: UTCTime
  , renewalRenewalStatus :: Int
  , renewalComment :: T.Text
  -- ^ Reason for non-renewal if
  -- renewalStatus == 1.
  -- Otherwise is comment if
  -- renewalStatus == 0.
  }

-- | Events that trigger notifications.
data Trigger
  = All
  | OnlyFailure
  | WeeklyDigest
  | MonthlyDigest
  | Never

triggerToInt :: Trigger -> Int
triggerToInt t = case t of
  All -> 1
  OnlyFailure -> 2
  WeeklyDigest -> 3
  MonthlyDigest -> 4
  Never -> 5

instance FromJSON Trigger where
  parseJSON (String s) = pure $ case s of
    "all" -> All
    "onlyFailure" -> OnlyFailure
    "weekly" -> WeeklyDigest
    "monthly" -> MonthlyDigest
    "never" -> Never
    _ -> error $ "Not recognized notification trigger: " ++ T.unpack s
  parseJSON _ = error "Expected string JSON value given while \
                          \parsing notification trigger."

instance ToJSON Trigger where
  toJSON t = String $ case t of
    All -> "all"
    OnlyFailure -> "onlyFailure"
    WeeklyDigest -> "weekly"
    MonthlyDigest -> "monthly"
    Never -> "never"

instance FromJSON TokenId where
  parseJSON (Object o) = TokenId <$> o .: "stripeToken"

-- | Returned to the registrant upon first renewal.
-- Represents the current state of the registrant's
-- profile with only renewal-related information.
-- This is also returned to the registrant
-- when he/she looks up his/her own profile page to
-- look at book renewal state.
data RenewalProfile
  = RenewalProfile
  { bookList :: [DBBook]
  }
  deriving (FromJSON, ToJSON, Generic)

-- | A 'DBBook' contains its description, and some renewal
-- associated information. It is the representation of the
-- book in the database.
data DBBook
  = DBBook
  { dbDescription :: T.Text
  , dbItemStatus :: T.Text
  , dbDueDate :: UTCTime
  , dbLastRenewed :: UTCTime
  }
  deriving (FromJSON, ToJSON, Generic)

-- | Returned the registrant upon initial
-- registration. Contains some extra information
-- over the 'RenewalProfile'.
data DetailedProfile
  = DetailedProfile
  { detailedBookList :: [DetailedBook]
  }
  deriving (FromJSON, ToJSON, Generic)

-- | Detailed book data from just examining loan status.
data DetailedBook
  = DetailedBook
  { author :: T.Text
  , description :: T.Text
  , year :: T.Text
  , library :: T.Text
  , callNumber :: T.Text
  , dueDate :: T.Text
  , dueHour :: T.Text
  , accruingFine :: T.Text
  , numberOfRenewals :: T.Text
  }
  deriving (FromJSON, ToJSON, Generic)
