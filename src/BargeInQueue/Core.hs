{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module BargeInQueue.Core
    ( NewWorkItem(..)
    , nwiId
    , nwiName
    , nwiSystemId
    , nwiWorkerType
    , nwiGroupId
    , nwiDependsOnGroups
    , nwiDependsOnWorkItem
    , nwiOverrideIgnoreUntil
    , nwiOverrideRetriesLeft

    , SystemConfig(..)
    , sysId
    , sysRequiresGlobalLock
    , sysPollPeriodSeconds
    , sysLockedUntil
    , sysLockedBy

    , WorkItem(..)
    , wiId
    , wiSystemId
    , wiName
    , wiWorkerType
    , wiIgnoreUntil
    , wiRetriesLeft
    , wiCreatedAt
    , wiGroupId
    , wiDependsOnGroups
    , wiDependsOnWorkItem
    , wiBackoffCount
    , wiAttempts

    , DequeuedActiveItem(..)
    , dqaQueueId
    , dqaPendingItemId
    , dqaWorkItemId
    , dqaWorkTypeId
    , dqaWorkItemName
    , dqaDequeuedAt
    , dqaWorkData

    , QueueItemId(..)
    , WorkItemId(..)
    , PendingWorkItemId(..)
    , SystemId(..)
    , WorkTypeId(..)
    , PendingWorkItems(..)
    , QueueWorkItems(..)
    ) where

import           Verset
import           Control.Lens (makeLenses)

newtype QueueItemId = QueueItemId Int deriving (Show, Eq)
newtype WorkItemId = WorkItemId UUID deriving (Show, Eq)
newtype PendingWorkItemId = PendingWorkItemId Int deriving (Show, Eq)
newtype SystemId = SystemId UUID deriving (Show, Eq)
newtype WorkTypeId = WorkTypeId UUID deriving (Show, Eq)
newtype PendingWorkItems = PendingWorkItems [NewWorkItem] deriving (Show, Eq)
newtype QueueWorkItems = QueueWorkItems [NewWorkItem] deriving (Show, Eq)


data NewWorkItem = NewWorkItem
  { _nwiId :: !WorkItemId
  , _nwiName :: !Text
  , _nwiSystemId :: !SystemId
  , _nwiWorkerType :: !WorkTypeId
  , _nwiGroupId :: !(Maybe UUID)
  , _nwiDependsOnGroups :: ![UUID]
  , _nwiDependsOnWorkItem :: ![UUID]
  , _nwiOverrideIgnoreUntil :: !(Maybe UTCTime)
  , _nwiOverrideRetriesLeft :: !(Maybe Int)
  } deriving (Show, Eq)


data SystemConfig = SystemConfig
  { _sysId :: !SystemId
  , _sysRequiresGlobalLock :: !Bool
  , _sysPollPeriodSeconds :: !Int
  , _sysLockedUntil :: !(Maybe UTCTime)
  , _sysLockedBy :: !(Maybe Text)
  } deriving (Show)


data WorkItem = WorkItem
  { _wiId :: !WorkItemId
  , _wiSystemId :: !SystemId
  , _wiName :: !Text
  , _wiWorkerType :: !WorkTypeId
  , _wiIgnoreUntil :: !(Maybe UTCTime)
  , _wiRetriesLeft :: !Int
  , _wiCreatedAt :: !UTCTime
  , _wiGroupId :: !(Maybe UUID)
  , _wiDependsOnGroups :: ![UUID]
  , _wiDependsOnWorkItem :: ![UUID]
  , _wiBackoffCount :: !Int
  , _wiAttempts :: !Int
  }


data DequeuedActiveItem = DequeuedActiveItem
  { _dqaQueueId :: !QueueItemId
  , _dqaPendingItemId :: !PendingWorkItemId
  , _dqaWorkItemId :: !WorkItemId
  , _dqaWorkTypeId :: !WorkTypeId
  , _dqaWorkItemName :: !Text
  , _dqaDequeuedAt :: !(Maybe UTCTime)
  , _dqaWorkData :: !(Maybe Text)
  } deriving (Show)


makeLenses ''DequeuedActiveItem
makeLenses ''NewWorkItem
makeLenses ''WorkItem
makeLenses ''SystemConfig
