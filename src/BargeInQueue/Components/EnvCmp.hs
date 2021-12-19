{-# LANGUAGE NoImplicitPrelude #-}

module BargeInQueue.Components.EnvCmp
    ( EnvCmp(..)
    ) where

import           Verset
import qualified BargeInQueue.Core as C
import qualified BargeInQueue.Components.UserCmp as CUsr

-- | State. This could have also just been an Env and the components specialised to ReaderT IO Env, but may as well stick with the pattern
data EnvCmp m = EnvCmp
  { envIsStarted :: !(m Bool)
  , envSetStarted :: !(CUsr.UserCmp m -> m ())
  , envSystem :: !C.SystemConfig

  , envDemandUser :: !(m (CUsr.UserCmp m)) -- ^ Gets the UserCmp or throws if one does not exist
  }
