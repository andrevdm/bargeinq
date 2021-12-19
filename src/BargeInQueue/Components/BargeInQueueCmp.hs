{-# LANGUAGE NoImplicitPrelude #-}

module BargeInQueue.Components.BargeInQueueCmp
    ( BargeInQueueCmp(..)
    ) where

import           Verset

import qualified BargeInQueue.Core as C
import qualified BargeInQueue.Components.UserCmp as CUsr

data BargeInQueueCmp m = BargeInQueueCmp
  { bqVersion :: !Text
  , bqStartQueue :: !(CUsr.UserCmp m -> m ())
  , bqQueueWork :: !(C.PendingWorkItems -> C.QueueWorkItems -> m ())
  }
