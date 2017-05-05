{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Renewal.DB where

import Renewal.Types

import Control.Monad ( void )
import Data.Aeson ( FromJSON, ToJSON )
import Data.Maybe ( listToMaybe )
import Data.String ( fromString )
import Data.Time.Clock ( UTCTime, NominalDiffTime )
import Data.Time.LocalTime
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow

newtype ProfileId = ProfileId { unProfileId :: Int }
  deriving (FromField, ToField, FromJSON, ToJSON)

newtype RenewalId = RenewalId { unRenewalId :: Int }
  deriving (FromField, ToField, FromJSON, ToJSON)

newtype RenewalItemId = RenewalItemId { unRenewalItemId :: Int }
  deriving (FromField, ToField, FromJSON, ToJSON)

newtype NotificationSettingId
  = NotificationSettingId
    { unNotificationSettingId :: Int
    }
  deriving (FromField, ToField, FromJSON, ToJSON)

data DBProfile
  = DBProfile
    { dbProfileId :: ProfileId
    , dbProfileUsername :: Username
    , dbProfilePassword :: Password
    , dbProfileEmail :: Maybe Email
    , dbProfilePhoneNumber :: Maybe PhoneNumber
    , dbProfileCreatedAt :: UTCTime
    , dbProfileServiceExpiry :: Maybe UTCTime
    }

instance FromRow DBProfile where
  fromRow = DBProfile
    <$> field
    <*> field
    <*> field
    <*> field
    <*> field
    <*> (localTimeToUTC utc <$> field)
    <*> (fmap (localTimeToUTC utc) <$> field)

-- | Add 'DiffTime' expiry to 'Username' servicable period.
updateServiceExpiry :: Username -> NominalDiffTime -> Connection -> IO ()
updateServiceExpiry u d dbconn = void (execute dbconn q (Only u)) where
  q = fromString $
    "update profile set \
    \(service_expiry) = (now() + interval '"
    ++ (show $ floor d)
    ++ " seconds') \
    \where username = ?"

-- | Creates a new payment record for the given user.
createPayment :: Username -> Int -> Connection -> IO ()
createPayment u amt dbconn = void (execute dbconn q (Only u)) where
  q = fromString $
    "insert into payment (profile_id, amount_cts) \
    \(select id, " ++ show amt ++ " from profile \
    \where username = ?)"

getRenewalProfileQuery :: Query
getRenewalProfileQuery = "select i.description, i.item_status, i.due_date, r.created_at from profile p inner join renewal r on (r.profile_id = p.id) inner join renewal_item i on (i.renewal_id = r.id) where p.username = ? and r.created_at = (select max(r.created_at) from renewal r inner join profile p on (p.id = r.profile_id) where p.username = ?)"

-- | Return the current 'RenewalProfile' of the given 'Username'
-- based on the current database status.
getRenewalProfile :: Username -> Connection -> IO RenewalProfile
getRenewalProfile u dbconn = do
  rows {- :: [(T.Text, T.Text, LocalTime, LocalTime)] -} <-
    query dbconn getRenewalProfileQuery (u, u)
  let f = localTimeToUTC utc
  pure $ RenewalProfile $
    rows <&> (\(desc, stat, due, l) -> DBBook desc stat (f due) (f l))

getPassword :: Username -> Connection -> IO (Maybe Password)
getPassword u dbconn
  = fmap fromOnly . listToMaybe <$> query dbconn q (Only u) where
    q = "select password from profile where username = ?"

createRenewal :: Username -> Connection -> IO RenewalId
createRenewal u dbconn
  = fromOnly . head <$> query dbconn q (Only u) where
    q =
      "insert into renewal (profile_id) \
      \(select id from profile where username = ?) returning id"

renewalItemTuple
  :: Monad m
  => RenewalId
  -> RenewalResult
  -> m (RenewalId, Description, ItemStatus, UTCTime, Int, Comment)
renewalItemTuple rid r@RenewalResult{..} = do
  time <- parsedRenewalDueDate r
  pure
    ( rid
    , renewalDescription
    , renewalItemStatus
    , time
    , renewalStatusToInt RenewalSuccess -- XXX we should actually check somehow
    , renewalComment
    )

createRenewalItem
  :: RenewalId
  -> RenewalResult
  -> Connection
  -> IO RenewalItemId
createRenewalItem rid r dbconn = m where
  m = fromOnly . head <$> query dbconn q t
  r' = renewalItemTuple rid r
  t = maybe (error "failed to prepare renewalresult)") id r'
  q =
    "insert into renewal_item \
    \(renewal_id, description, item_status, due_date, renewal_status, comment) \
    \values (?, ?, ?, ?, ?, ?)"

createRenewalItems
  :: RenewalId
  -> [RenewalResult]
  -> Connection
  -> IO [RenewalItemId]
createRenewalItems rid params dbconn = m where
  m = fmap fromOnly <$> returning dbconn q params'
  unsafe = maybe (error "failed to parse") id . renewalItemTuple rid
  params' = map unsafe params
  q =
    "insert into renewal_item \
    \(renewal_id, description, item_status, due_date, renewal_status, comment) \
    \ values (?, ?, ?, ?, ?, ?) returning id"

createProfile
  :: Username -> Password -> Email -> PhoneNumber -> Connection -> IO ProfileId
createProfile u p e n dbconn
  = fromOnly . head <$> query dbconn q (u, p, e, n) where
    q =
      "insert into profile (username, password, email_address, phone_number) \
      \ values (?, ?, ?, ?) returning id"

createNotificationSetting :: ProfileId -> Trigger -> Connection -> IO ()
createNotificationSetting p t dbconn = void m where
  m = execute dbconn q (p, triggerToInt t)
  q =
    "insert into notification_setting (profile_id, notification_level) \
    \values (?, ?)"

getAllActiveUsers :: Connection -> IO [DBProfile]
getAllActiveUsers dbconn = query_ dbconn q where
  q =
    "select id, username, password, email_address, phone_number, created_at, service_expiry \
    \from profile \
    \where service_expiry > now()"

-- | Check whether a user has paid for service.
--
-- Returns 'Nothing' if the user doesn't exist.
isUserActive :: Username -> Connection -> IO (Maybe Bool)
isUserActive u dbconn = fmap fromOnly . listToMaybe <$> m where
  m = query dbconn q (Only u)
  q =
    "select service_expiry is not null and now() < service_expiry from profile \
    \where username = ?"
