{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module BargeInQueue.Impl.UuidCmpIO
    ( newUuidCmpIO
    ) where

import           Verset
import qualified Data.UUID.V4 as UU

import qualified BargeInQueue.Components.UuidCmp as CUu


newUuidCmpIO :: forall m. (MonadIO m) => CUu.UuidCmp m
newUuidCmpIO =
  CUu.UuidCmp
    { CUu.uuRandomUuid = liftIO UU.nextRandom
    }
