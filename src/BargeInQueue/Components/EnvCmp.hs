{-# LANGUAGE NoImplicitPrelude #-}

module BargeInQueue.Components.EnvCmp
    ( EnvCmp(..)
    ) where

import           Verset
import qualified BargeInQueue.Core as C

-- | State. This could have also just been an Env and the components specialised to ReaderT IO Env, but may as well stick with the pattern
data EnvCmp m = EnvCmp
  { envIsStarted :: !(m Bool)
  , envSetStarted :: !(m ())
  , envSystem :: !C.SystemConfig
  }
