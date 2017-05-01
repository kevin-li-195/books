{#- LANGUAGE DataKind #-}
{#- LANGUAGE TypeOperators #-}

module Server.Types where

import Servant.Api
import qualified Data.Text as T

type RenewalApi
  = "register" 
  :> ReqBody '[JSON] Registrant
  :> Post '[JSON] Profile

data Registrant
  = Registrant
  { email :: T.Text
  , notificationEmail :: T.Text
  , pass :: T.Text
  , settings :: RegSettings
  }

-- | Registrant settings upon registration.
data RegSettings
  = RegSettings
  { mcgillEmailNotify :: Bool
  , notificationTrigger :: NotificationTrigger
  }

-- | Events that trigger notifications.
data NotificationTrigger
  = All
  | OnlyFailure
  | WeeklyDigest
  | MonthlyDigest
  | Never
