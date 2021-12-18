{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Components.BargeInQueueCmp
    ( BargeInQueueCmp(..)
    ) where

import           Protolude

import qualified Components.QueueCmp as CQ

data BargeInQueueCmp m = BargeInQueueCmp
  { bqQueueWork :: !(CQ.PendingWorkItems -> CQ.QueueWorkItems -> m ())
  }
