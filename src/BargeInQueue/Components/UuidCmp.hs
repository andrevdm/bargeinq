{-# LANGUAGE NoImplicitPrelude #-}

module BargeInQueue.Components.UuidCmp
    ( UuidCmp(..)
    ) where

import           Data.UUID (UUID)


newtype UuidCmp m = UuidCmp { uuRandomUuid :: m UUID }
