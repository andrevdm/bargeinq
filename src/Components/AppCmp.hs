{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Components.AppCmp
    ( AppCmp(..)
    ) where

import           Protolude

import qualified Components.QueueCmp as Q

data AppCmp m = AppCmp
  { appRun :: Q.SystemId -> m Q.QueueHandle
  , appText :: m Text
  }
