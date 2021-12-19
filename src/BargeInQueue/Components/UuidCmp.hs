{-# LANGUAGE NoImplicitPrelude #-}

module BargeInQueue.Components.UuidCmp
    ( UuidCmp(..)
    ) where

import           Verset


newtype UuidCmp m = UuidCmp { uuRandomUuid :: m UUID }
