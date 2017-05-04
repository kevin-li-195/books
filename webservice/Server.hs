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

import Config
import qualified DB as DB

import Renewal.Types
import qualified Renewal.LibraryScraper as Library

import Control.Monad
import Control.Monad.Trans
import qualified Data.Text as T
import Servant
import qualified System.IO as IO
import Web.Stripe.Charge
import Web.Stripe

renewalServer :: Config -> Server RenewalApi
renewalServer cfg = register cfg :<|> pay cfg :<|> echo where
  echo :: T.Text -> Handler T.Text
  echo = pure

-- | Register the 'Registrant' in the database,
-- and return the freshly generated detailed profile.
-- from immediately scraping the library webpage.
register :: Config -> Registrant -> Handler DetailedProfile
register conf Registrant{..} = do
  let username = registrantUsername
  let email = notificationEmail

  liftIO (Library.checkUser username pass) >>= \case
    Right books -> liftIO $ do
      profileId <- DB.createProfile username pass email phoneNumber conf
      void $ DB.createNotificationSetting profileId trigger conf

      pure (DetailedProfile books)
    Left err -> fail (Library.formatError err)

-- | Base 1.00 CAD payment.
pmt :: StripeRequest CreateCharge
pmt = createCharge (Amount chargeAmountCts) CAD

pay :: Config -> PaymentInfo -> Handler RenewalProfile
pay conf@Config{..} PaymentInfo{..} = do
  liftIO $ IO.hPrint IO.stderr (id @String "Hello there.")
  result <- liftIO $ stripe stripeConfig $ pmt -&- tokenId
  case result of
    Right details
      | chargePaid details -> liftIO $ do
        IO.hPrint IO.stderr $ id @String "Marking account paid."
        DB.createPayment paymentUsername conf

        IO.hPrint IO.stderr $ id @String "Updating service expiry."
        DB.updateServiceExpiry paymentUsername conf

        IO.hPrint IO.stderr $ id @String "Renewing."
        _ <- renew paymentUsername conf

        IO.hPrint IO.stderr $ id @String "Getting renewal profile."
        rp <- DB.getRenewalProfile paymentUsername conf

        IO.hPrint IO.stderr $ id @String "Got renewal profile."
        pure rp

      | otherwise -> fail $ "Charge was not paid." ++ (show details)
    Left err -> fail $ show err

-- | Try to renew all books of given 'Username' and return results.
renew :: Username -> Config -> IO [RenewalResult]
renew u conf@Config{..} = do
  pass <- maybe (fail "unknown username") pure =<< DB.getPassword u conf

  liftIO (Library.renew u pass) >>= \case
    Right results -> liftIO $ do
      renewalId <- DB.createRenewal u conf
      _ <- DB.createRenewalItems renewalId results conf
      pure results
    Left e -> fail (Library.formatError e)
