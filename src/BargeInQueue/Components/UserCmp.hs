{-# LANGUAGE NoImplicitPrelude #-}

module BargeInQueue.Components.UserCmp
    ( UserCmp(..)
    , ProcessItemResult(..)
    ) where

import           Verset

import qualified BargeInQueue.Core as CR


data ProcessItemResult
  = PirSuccess
  | PirError Text Text
  deriving (Show, Eq)


data UserCmp m = UserCmp
  { usrQueueStarting :: m ()
  , usrProcessActiveItem :: !(CR.QueueItemId -> CR.WorkItemId -> CR.WorkTypeId -> Text -> m ProcessItemResult)
  }
