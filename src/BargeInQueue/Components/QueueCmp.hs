{-# LANGUAGE NoImplicitPrelude #-}

module BargeInQueue.Components.QueueCmp
    ( QueueCmp(..)
    ) where

import           Verset

import qualified BargeInQueue.Core as C


data QueueCmp m = QueueCmp
  { qStartQueue :: !(m ())
  , qQueueWork :: !(C.PendingWorkItems -> C.QueueWorkItems -> m ())
  }

