{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module BargeInQueue.Components.BargeInQueueCmp
    ( BargeInQueueCmp(..)
    ) where

import           Verset

import qualified BargeInQueue.Core as C

data BargeInQueueCmp m = BargeInQueueCmp
  { bqVersion :: !Text
  , bqQueueWork :: !(C.PendingWorkItems -> C.QueueWorkItems -> m ())
  }
