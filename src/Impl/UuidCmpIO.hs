{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Impl.UuidCmpIO
    ( newUuidCmpIO
    ) where

import           Protolude
import qualified Data.UUID.V4 as UU

import qualified Components.UuidCmp as CUu


newUuidCmpIO :: forall m. (MonadIO m) => CUu.UuidCmp m
newUuidCmpIO =
  CUu.UuidCmp
    { CUu.uuRandomUuid = liftIO UU.nextRandom
    }
