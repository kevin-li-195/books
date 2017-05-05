{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Renewal.Types where

import Control.Monad.Reader

import Data.Aeson
import qualified Data.ByteString as BS
import Data.Pool
import Data.Proxy ( Proxy(Proxy) )
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format ( defaultTimeLocale, parseTimeM )
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import GHC.Generics
import Servant.API
import Web.Stripe
import qualified Web.Stripe.Charge as Stripe

type RenewalApi
  = "register"
    :> ReqBody '[JSON] Registrant
    :> Post '[JSON] DetailedProfile
  :<|> "payment"
    :> ReqBody '[JSON] PaymentInfo
    :> Post '[JSON] RenewalProfile
  :<|> "checkUser"
    :> ReqBody '[JSON] Username
    :> Post '[JSON] Bool

renewalApi :: Proxy RenewalApi
renewalApi = Proxy

-- | Error types in 'BookRenewalMonad'.
data RenewalError
  = Unknown String
  | NoSuchUser Username

data PaymentInfo
  = PaymentInfo
  { tokenId :: Stripe.TokenId
  , paymentUsername :: Username
  }
  deriving (Eq, Ord, Read, Show)

instance FromJSON PaymentInfo where
  parseJSON (Object o) = PaymentInfo
    <$> o .: "tokenId"
    <*> o .: "paymentUsername"

-- | New type wrapper for usernames.
newtype Username = Username { unUsername :: T.Text }
  deriving (Eq, Ord, Read, Show, FromField, ToField, FromJSON, ToJSON)

-- | Wrapper for passwords.
newtype Password = Password { unPassword :: T.Text }
  deriving (Eq, Ord, Read, Show, FromField, ToField, FromJSON, ToJSON)

newtype Email = Email { unEmail :: T.Text }
  deriving (Eq, Ord, Read, Show, FromField, ToField, FromJSON, ToJSON)

newtype PhoneNumber = PhoneNumber { unPhoneNumber :: T.Text }
  deriving (Eq, Ord, Read, Show, FromField, ToField, FromJSON, ToJSON)

-- | Data provided by the registrant.
data Registrant
  = Registrant
  { registrantUsername :: Username
  -- ^ McGill email
  , notificationEmail :: Email
  -- ^ Optional notification email
  , pass :: Password
  -- ^ McGill email password
  , phoneNumber :: PhoneNumber
  -- ^ Phone number for notifications
  , trigger :: Trigger
  -- ^ Trigger for notifications
  }
  deriving (Eq, Ord, Read, Show)

instance FromJSON Registrant where
  parseJSON (Object o) = Registrant
    <$> o .: "registrantUsername"
    <*> o .: "notificationEmail"
    <*> o .: "pass"
    <*> o .: "phoneNumber"
    <*> o .: "trigger"
  parseJSON _ = fail "cannot parse registrant from non-object"

instance ToJSON Registrant where
  toJSON Registrant{..} = object
    [ "registrantUsername" .= registrantUsername
    , "notificationEmail" .= notificationEmail
    , "pass" .= pass
    , "phoneNumber" .= phoneNumber
    , "trigger" .= trigger
    ]

-- | Server config.
data ServerConfig
  = ServerConfig
  { serverStripeConfig :: StripeConfig
  , serverDbconn :: Connection
  , serverPayment :: Int
  -- ^ Number of cents
  , serverServiceTime :: NominalDiffTime
  -- ^ Amount of time that we provide service
  }

data RequestConfig
  = RequestConfig
  { reqStripeConfig :: StripeConfig
  , reqDbconn :: Pool Connection
  , reqPayment :: Int
  -- ^ Number of cents
  , reqServiceTime :: NominalDiffTime
  -- ^ Amount of time that we provide service
  }

-- | Connects to the DB with the given connection string and uses the given
-- bytestring as the stripe key.
newConfig
    :: BS.ByteString
    -> BS.ByteString
    -> Int
    -- ^ Number of cents for payment
    -> NominalDiffTime
    -- ^ Amount of time for service
    -> IO RequestConfig
newConfig connstr key pmt diff = do
  connPool <- createPool (connectPostgreSQL connstr) close 1 10 10
  pure RequestConfig
    { reqDbconn = connPool
    , reqStripeConfig = StripeConfig (StripeKey key)
    , reqPayment = pmt
    , reqServiceTime = diff
    }

newtype Description = Description { unDescription :: T.Text }
  deriving (Eq, Ord, Read, Show, FromField, ToField, FromJSON, ToJSON)

newtype ItemStatus = ItemStatus { unItemStatus :: T.Text }
  deriving (Eq, Ord, Read, Show, FromField, ToField, FromJSON, ToJSON)

newtype Comment = Comment { unComment :: T.Text }
  deriving (Eq, Ord, Read, Show, FromField, ToField, FromJSON, ToJSON)

-- | Single row in renewal output.
data RenewalResult
  = RenewalResult
  { renewalDescription :: Description
  , renewalItemStatus :: ItemStatus
  , renewalDueDate :: T.Text
  , renewalDueHour :: T.Text
  , renewalLibrary :: T.Text
  , renewalBarcode :: T.Text
  , renewalItemDesc :: T.Text
  , renewalComment :: Comment
  -- ^ Reason for non-renewal if
  -- renewal failed.
  -- Otherwise is comment.
  }
  deriving Show

parsedRenewalDueDate :: Monad m => RenewalResult -> m UTCTime
parsedRenewalDueDate RenewalResult{..}
  = parseTimeM True defaultTimeLocale "%B %e %Y %R"
    (T.unpack renewalDueDate ++ " " ++ T.unpack renewalDueHour)

-- | Events that trigger notifications.
data Trigger
  = All
  | OnlyFailure
  | WeeklyDigest
  | MonthlyDigest
  | Never
  deriving (Eq, Ord, Read, Show)

triggerToInt :: Trigger -> Int
triggerToInt t = case t of
  All -> 1
  OnlyFailure -> 2
  WeeklyDigest -> 3
  MonthlyDigest -> 4
  Never -> 5

data RenewalStatus
  = RenewalSuccess
  | RenewalFailure

renewalStatusToInt :: RenewalStatus -> Int
renewalStatusToInt r = case r of
  RenewalSuccess -> 0
  RenewalFailure -> 1

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

instance FromJSON Stripe.TokenId where
  parseJSON (String s) = pure (Stripe.TokenId s)
  parseJSON _ = error "Expected string JSON value given."

-- | Returned to the registrant upon first renewal.
-- Represents the current state of the registrant's
-- profile with only renewal-related information.
-- This is also returned to the registrant
-- when he/she looks up his/her own profile page to
-- look at book renewal state.
newtype RenewalProfile
  = RenewalProfile
  { bookList :: [DBBook]
  }
  deriving (Eq, Ord, Read, Show)

instance ToJSON RenewalProfile where
  toJSON RenewalProfile{..} = object [ "bookList" .= bookList ]

-- | A 'DBBook' contains its description, and some renewal
-- associated information. It is the representation of the
-- book in the database.
data DBBook
  = DBBook
  { dbDescription :: Description
  , dbItemStatus :: ItemStatus
  , dbDueDate :: UTCTime
  , dbLastRenewed :: UTCTime
  }
  deriving (Eq, Ord, Read, Show)

instance ToJSON DBBook where
  toJSON DBBook{..} = object
    [ "dbDescription" .= dbDescription
    , "dbItemStatus" .= dbItemStatus
    , "dbDueDate" .= dbDueDate
    , "dbLastRenewed" .= dbLastRenewed
    ]

-- | Returned the registrant upon initial
-- registration. Contains some extra information
-- over the 'RenewalProfile'.
newtype DetailedProfile
  = DetailedProfile
  { detailedBookList :: [DetailedBook]
  }
  deriving (Eq, Ord, Read, Show)

instance ToJSON DetailedProfile where
  toJSON DetailedProfile{..} = object [ "detailedBookList" .= detailedBookList ]

-- | Detailed book data from just examining loan status.
data DetailedBook
  = DetailedBook
  { author :: T.Text
  , description :: Description
  , year :: T.Text
  , library :: T.Text
  , callNumber :: T.Text
  , dueDate :: T.Text
  , dueHour :: T.Text
  , accruingFine :: T.Text
  , numberOfRenewals :: T.Text
  }
  deriving (Eq, Ord, Read, Show)

instance ToJSON DetailedBook where
  toJSON DetailedBook{..} = object
    [ "author" .= author
    , "description" .= description
    , "year" .= year
    , "library" .= library
    , "callNumber" .= callNumber
    , "dueDate" .= dueDate
    , "dueHour" .= dueHour
    , "accruingFine" .= accruingFine
    , "numberOfRenewals" .= numberOfRenewals
    ]

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
infixl 5 <&>
