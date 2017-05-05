{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Prelude hiding ( log )

import qualified Renewal.LibraryScraper as Library
import Renewal.Types
import Renewal.Monad

import Servant

import Web.Stripe.Charge

checkUser :: RenewalMonad m => Username -> m Bool
checkUser = isUserActive

renewalServer :: forall m. RenewalMonad m => ServerT RenewalApi m
renewalServer 
  = register 
  :<|> pay 
  :<|> checkUser

-- | Register the 'Registrant' in the database,
-- and return the freshly generated detailed profile.
-- from immediately scraping the library webpage.
register 
  :: RenewalMonad m
  => Registrant 
  -> m DetailedProfile
register Registrant{..} = do
  let username = registrantUsername
  let email = notificationEmail

  lookupUserBooks username pass >>= \case
    Right books ->
      getPassword username >>= \case
        Just _ -> pure $ DetailedProfile books
        Nothing -> do
          profileId <- createProfile username pass email phoneNumber
          createNotificationSetting profileId trigger
          pure (DetailedProfile books)
    Left err -> fail (Library.formatError err)

pay 
  :: RenewalMonad m
  => PaymentInfo 
  -> m RenewalProfile
pay PaymentInfo{..} = do
  log "Hello there."
  charge tokenId >>= \case
    Right details
      | chargePaid details -> do
        log "Marking account paid."
        createPayment paymentUsername

        log "Updating service expiry."
        updateServiceExpiry paymentUsername

        log "Getting renewal profile."
        rp <- getRenewalProfile paymentUsername

        log "Got renewal profile."
        pure rp

      | otherwise -> fail $ "Charge was not paid." ++ (show details)
    Left err -> fail $ show err
