{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Impl.AppCmpIO
    ( newAppCmpIO
    ) where

import           Protolude

import qualified Components.AppCmp as CA
import qualified Components.QueueCmp as CQ

newAppCmpIO
  :: forall m.
     (Monad m)
  => CA.AppCmp m
newAppCmpIO =
  CA.AppCmp
    { CA.appRun = \_sysId -> pure $ CQ.QueueHandle 0
    , CA.appText = pure "TODO"
    }

