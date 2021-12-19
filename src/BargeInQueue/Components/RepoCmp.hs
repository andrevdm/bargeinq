{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module BargeInQueue.Components.RepoCmp
    ( RepoCmp(..)
    , DequeuedActiveItem(..)
    , dqaQueueId
    , dqaPendingItemId
    , dqaWorkItemId
    , dqaWorkTypeId
    , dqaWorkItemName
    , dqaDequeuedAt
    ) where

import           Verset
import           Control.Lens (makeLenses)

import qualified BargeInQueue.Core as C

data RepoCmp m = RepoCmp
  { rpListSystems :: !(m (Either Text [C.SystemConfig]))
  , rpGetSystem :: !(C.SystemId -> m (Either Text (Maybe C.SystemConfig)))
  , rpFetchNextActiveItem :: !(C.SystemConfig -> m (Either Text (Maybe DequeuedActiveItem)))
  }

data DequeuedActiveItem = DequeuedActiveItem
  { _dqaQueueId :: !C.QueueItemId
  , _dqaPendingItemId :: !C.PendingWorkItemId
  , _dqaWorkItemId :: !C.WorkItemId
  , _dqaWorkTypeId :: !C.WorkTypeId
  , _dqaWorkItemName :: !Text
  , _dqaDequeuedAt :: !(Maybe UTCTime)
  } deriving (Show)


makeLenses ''DequeuedActiveItem
