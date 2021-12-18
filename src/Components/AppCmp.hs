{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Components.AppCmp
    ( AppCmp(..)
    ) where

import           Protolude
import qualified UnliftIO.Async as UA

import qualified Components.QueueCmp as Q

data AppCmp m = AppCmp
  { appRunQueue :: !(Q.SystemId -> m (UA.Async ()))
  }
