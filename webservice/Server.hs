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

import Renewal.Config
import qualified Renewal.DB as DB
import qualified Renewal.LibraryScraper as Library
import Renewal.Types

import Control.Monad
import Control.Monad.Trans
import qualified Data.Text as T
import Database.PostgreSQL.Simple ( Connection )

import Servant
import Servant.Server.Internal.ServantErr

import qualified System.IO as IO
import Web.Stripe.Charge
import Web.Stripe

renewalServer :: Config -> Server RenewalApi
renewalServer conf@Config{..} = register dbconn :<|> pay conf :<|> echo where
  echo :: T.Text -> Handler T.Text
  echo = pure

-- | Register the 'Registrant' in the database,
-- and return the freshly generated detailed profile.
-- from immediately scraping the library webpage.
register :: Connection -> Registrant -> Handler DetailedProfile
register dbconn Registrant{..} = do
  let username = registrantUsername
  let email = notificationEmail

  liftIO (Library.checkUser username pass) >>= \case
    Right books ->
      liftIO (DB.getPassword username conf) >>= \case
        Just pass -> throwError $ err409
            { errBody = "Username already registered."
            }
        Nothing -> liftIO $ do
          profileId <- DB.createProfile username pass email phoneNumber conf
          void $ DB.createNotificationSetting profileId trigger conf
          pure (DetailedProfile books)
    Left err -> fail (Library.formatError err)

-- | Base 1.00 CAD payment.
pmt :: StripeRequest CreateCharge
pmt = createCharge (Amount chargeAmountCts) CAD

pay :: Config -> PaymentInfo -> Handler RenewalProfile
pay Config{..} PaymentInfo{..} = do
  liftIO $ IO.hPrint IO.stderr (id @String "Hello there.")
  result <- liftIO $ stripe stripeConfig $ pmt -&- tokenId
  case result of
    Right details
      | chargePaid details -> liftIO $ do
        IO.hPrint IO.stderr $ id @String "Marking account paid."
        DB.createPayment paymentUsername dbconn

        IO.hPrint IO.stderr $ id @String "Updating service expiry."
        DB.updateServiceExpiry paymentUsername dbconn

        IO.hPrint IO.stderr $ id @String "Renewing."
        _ <- renew paymentUsername dbconn

        IO.hPrint IO.stderr $ id @String "Getting renewal profile."
        rp <- DB.getRenewalProfile paymentUsername dbconn

        IO.hPrint IO.stderr $ id @String "Got renewal profile."
        pure rp

      | otherwise -> fail $ "Charge was not paid." ++ (show details)
    Left err -> fail $ show err

-- | Try to renew all books of given 'Username' and return results.
renew :: Username -> Connection -> IO [RenewalResult]
renew u dbconn = do
  pass <- maybe (fail "unknown username") pure =<< DB.getPassword u dbconn

  liftIO (Library.renew u pass) >>= \case
    Right results -> liftIO $ do
      renewalId <- DB.createRenewal u dbconn
      _ <- DB.createRenewalItems renewalId results dbconn
      pure results
    Left e -> fail (Library.formatError e)
