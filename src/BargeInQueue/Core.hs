{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module BargeInQueue.Core
    ( NewWorkItem(..)
    , nwiId
    , nwiName
    , nwiSystemId
    , nwiWorkerType
    , nwiGroupId
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
    , wiWorkerTypeId
    , wiIgnoreUntil
    , wiRetriesLeft
    , wiCreatedAt
    , wiGroupId
    , wiBackoffCount
    , wiAttempts
    , wiData

    , DequeuedActiveItem(..)
    , dqaQueueId
    , dqaPendingItemId
    , dqaWorkItemId
    , dqaWorkTypeId
    , dqaWorkItemName
    , dqaDequeuedAt
    , dqaWorkData

    , WorkType(..)
    , wtId
    , wtSystemId
    , wtName
    , wtDefaultRetries
    , wtDefaultBackoffSeconds
    , wtDefaultHeartbeatCheckPeriod
    , wtDefaultExecEnvironment
    , wtDequeueLockPeriodSeconds

    , QueueItemId(..)
    , WorkItemId(..)
    , PendingWorkItemId(..)
    , SystemId(..)
    , WorkTypeId(..)
    , GroupId(..)
    , PendingWorkItems(..)
    , QueueWorkItems(..)
    ) where

import           Verset
import           Control.Lens (makeLenses)

newtype QueueItemId = QueueItemId Int deriving (Show, Eq)
newtype WorkItemId = WorkItemId UUID deriving (Show, Eq)
newtype PendingWorkItemId = PendingWorkItemId Int deriving (Show, Eq)
newtype SystemId = SystemId UUID deriving (Show, Eq, Ord)
newtype WorkTypeId = WorkTypeId UUID deriving (Show, Eq, Ord)
newtype PendingWorkItems = PendingWorkItems [NewWorkItem] deriving (Show, Eq)
newtype QueueWorkItems = QueueWorkItems [NewWorkItem] deriving (Show, Eq)
newtype GroupId = GroupId UUID deriving (Show, Eq)

data WorkType = WorkType
  { _wtId :: !WorkTypeId
  , _wtSystemId :: !SystemId
  , _wtName :: !Text
  , _wtDefaultRetries :: !Int
  , _wtDefaultBackoffSeconds :: ![Int]
  , _wtDefaultHeartbeatCheckPeriod :: !(Maybe Int)
  , _wtDefaultExecEnvironment :: !Text
  , _wtDequeueLockPeriodSeconds :: !Int
  } deriving (Show, Eq, Ord)

data NewWorkItem = NewWorkItem
  { _nwiId :: !WorkItemId
  , _nwiName :: !Text
  , _nwiSystemId :: !SystemId
  , _nwiWorkerType :: !WorkTypeId
  , _nwiGroupId :: !(Maybe GroupId)
  , _nwiDependsOnWorkItem :: ![WorkItemId]
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
  , _wiWorkerTypeId :: !WorkTypeId
  , _wiIgnoreUntil :: !(Maybe UTCTime)
  , _wiRetriesLeft :: !Int
  , _wiCreatedAt :: !UTCTime
  , _wiGroupId :: !(Maybe GroupId)
  , _wiBackoffCount :: !Int
  , _wiAttempts :: !Int
  , _wiData :: !(Maybe Text)
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
makeLenses ''WorkType
