{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Renewal.Monad where

import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Except

import Data.Pool
import Data.Time.Clock ( DiffTime )

import Database.PostgreSQL.Simple ( Connection )

import Renewal.Types
import qualified Renewal.DB as DB
import qualified Renewal.LibraryScraper as Library

import Data.Aeson

import Web.Stripe
import Web.Stripe.Charge hiding ( Email )
import Web.Stripe.Error

import System.IO

class (Monad m) => RenewalMonad m where
  updateServiceExpiry :: Username -> m ()
  -- ^ Add extra time to the servicable period for the given user.
  createPayment :: Username -> m ()
  -- ^ Log a successful payment by the given user.
  getRenewalProfile :: Username -> m RenewalProfile
  -- ^ Return the current 'RenewalProfile of the given user.
  getPassword :: Username -> m (Maybe Password)
  -- ^ Return the password of the given user if it exists.
  isUserActive :: Username -> m Bool
  -- ^ Return true if user is active.
  createRenewal :: Username -> m DB.RenewalId
  -- ^ Create a 'batch-renewal' for the given user and returns
  -- its ID.
  createRenewalItems :: DB.RenewalId -> [RenewalResult] -> m [DB.RenewalItemId]
  -- ^ Create a list of 'RenewalItem' given results and the id
  -- of the corresponding batch renewal.
  createProfile 
    :: Username 
    -> Password
    -> Email
    -> PhoneNumber
    -> m DB.ProfileId
  -- ^ Create a profile for a registrant.
  createNotificationSetting
    :: DB.ProfileId
    -> Trigger
    -> m ()
  -- ^ Set the notification settings for the given user profile.
  getAllActiveUsers :: m [DB.DBProfile]
  -- ^ Get all active servicable users.
  lookupUserBooks
    :: Username
    -> Password
    -> m (Either Library.LibraryScraperInfo [DetailedBook])
  -- ^ Return the current list of books that a user
  -- has borrowed or an error.
  log :: String -> m ()
  -- ^ Print log
  charge 
    :: TokenId 
    -> m (Either StripeError (StripeReturn CreateCharge))
  -- ^ Charge a 'TokenId' the base 'Amount'

-- | Use connection to perform an action.
withConn 
  :: (MonadBaseControl IO m, MonadReader Config m) 
  => (Connection -> m a) -> m a
withConn f = do
  pool <- asks dbConnPool
  withResource pool f

newtype BookRenewalMonad a
  = BookRenewalMonad
  { runRenewal :: ExceptT RenewalError (ReaderT Config IO) a
  }
  deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadReader Config
    , MonadIO
    , MonadBase IO
    , MonadError RenewalError
    )

instance MonadBaseControl IO BookRenewalMonad where
  type StM BookRenewalMonad a = Config -> IO (Either RenewalError a)

  liftBaseWith f = liftIO (f nat)
    where nat ma = pure $ runReaderT (runExceptT $ runRenewal ma)

  restoreM stm = BookRenewalMonad (ExceptT (ReaderT stm))

instance RenewalMonad (BookRenewalMonad) where
  updateServiceExpiry u = do
    d <- asks serviceTime
    withConn $ liftIO . DB.updateServiceExpiry u d

  createPayment u = do
    i <- asks payment
    withConn $ liftIO . DB.createPayment u i

  getRenewalProfile u = withConn $ liftIO . DB.getRenewalProfile u

  getPassword u = withConn $ liftIO . DB.getPassword u

  createRenewal u = withConn $ liftIO . DB.createRenewal u

  createRenewalItems rid res 
    = withConn $ liftIO . DB.createRenewalItems rid res

  createProfile u p e phon 
    = withConn $ liftIO . DB.createProfile u p e phon

  createNotificationSetting pid t
    = withConn $ liftIO . DB.createNotificationSetting pid t

  isUserActive u = do
    withConn (liftIO . DB.isUserActive u) >>= \case
      Just t -> pure t
      Nothing -> throwError $ NoSuchUser u

  getAllActiveUsers = withConn $ liftIO . DB.getAllActiveUsers

  lookupUserBooks u p = liftIO $ Library.checkUser u p

  log = liftIO . hPrint stderr

  charge t = do
    conf <- asks stripeConfig
    pmt <- asks payment
    liftIO $ stripe conf $ createCharge (Amount pmt) CAD -&- t
