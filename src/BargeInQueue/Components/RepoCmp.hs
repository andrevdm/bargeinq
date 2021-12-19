{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module BargeInQueue.Components.RepoCmp
    ( RepoCmp(..)
    , SystemConfig(..)
    ) where

import           Verset
import qualified Data.Time as DT

import qualified BargeInQueue.Core as C

data RepoCmp m = RepoCmp
  { rpListSystems :: !(m (Either SomeException [SystemConfig]))
  , rpFetchSystem :: !(C.SystemId -> m (Maybe SystemConfig))
  }


data SystemConfig = SystemConfig
  { scId :: !UUID
  , scRequiresGlobalLock :: !Bool
  , scPollPeriodSeconds :: !Int
  , scLockedUntil :: !(Maybe DT.UTCTime)
  , scLockedBy :: !(Maybe Text)
  } deriving (Show)
