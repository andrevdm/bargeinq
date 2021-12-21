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
    , sysPollPeriodSeconds
    , sysLockedUntil
    , sysLockedBy
    , sysAutoQueueUnblocked
    , sysHeartbeatCheckPeriodSeconds

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
    , dqaWorkItemId
    , dqaWorkTypeId
    , dqaWorkItemName
    , dqaDequeuedAt
    , dqaWorkData
    , dqaFailReason

    , WorkType(..)
    , wtId
    , wtSystemId
    , wtName
    , wtDefaultRetries
    , wtDefaultBackoffSeconds
    , wtDefaultExecEnvironment
    , wtDequeueLockPeriodSeconds
    , wtHeartbeatSettings

    , HeartbeatSettings(..)
    , hbHeartbeatExpectedEverySeconds
    , hbHeartbeatNumMissedForError

    , QueueItemId(..)
    , WorkItemId(..)
    , SystemId(..)
    , WorkTypeId(..)
    , GroupId(..)
    , PendingWorkItems(..)
    , QueueWorkItems(..)

    , FailReason(..)
    , failReasonToId
    , failReasonFromId
    ) where

import           Verset
import           Control.Lens (makeLenses)

newtype QueueItemId = QueueItemId Int deriving (Show, Eq)
newtype WorkItemId = WorkItemId UUID deriving (Show, Eq)
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
  , _wtDefaultExecEnvironment :: !Text
  , _wtDequeueLockPeriodSeconds :: !Int
  , _wtHeartbeatSettings :: !(Maybe HeartbeatSettings)
  } deriving (Show, Eq, Ord)

data HeartbeatSettings = HeartbeatSettings
  { _hbHeartbeatExpectedEverySeconds :: !Int
  , _hbHeartbeatNumMissedForError :: !Int
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
  , _sysPollPeriodSeconds :: !Int
  , _sysLockedUntil :: !(Maybe UTCTime)
  , _sysLockedBy :: !(Maybe Text)
  , _sysMaxActiveItems :: !(Maybe Int)
  , _sysAutoQueueUnblocked :: !Bool
  , _sysHeartbeatCheckPeriodSeconds :: !(Maybe Int)
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
  } deriving (Show)


data DequeuedActiveItem = DequeuedActiveItem
  { _dqaQueueId :: !QueueItemId
  , _dqaWorkItemId :: !WorkItemId
  , _dqaWorkTypeId :: !WorkTypeId
  , _dqaWorkItemName :: !Text
  , _dqaDequeuedAt :: !(Maybe UTCTime)
  , _dqaWorkData :: !(Maybe Text)
  , _dqaFailReason :: !(Maybe FailReason)
  } deriving (Show)


data FailReason
  = FrError
  | FrHeartbeatTimeout
  | FrManualFail
  | FrManualExpire
  deriving (Show, Eq, Ord, Enum, Bounded)


failReasonToId :: FailReason -> Int
failReasonToId FrError = 543000
failReasonToId FrHeartbeatTimeout = 543001
failReasonToId FrManualFail = 543002
failReasonToId FrManualExpire = 543003

failReasonFromId :: Int -> FailReason
failReasonFromId 543001 = FrHeartbeatTimeout
failReasonFromId 543002 = FrManualFail
failReasonFromId 543003 = FrManualExpire
failReasonFromId _ = FrError


makeLenses ''DequeuedActiveItem
makeLenses ''NewWorkItem
makeLenses ''WorkItem
makeLenses ''SystemConfig
makeLenses ''WorkType
makeLenses ''HeartbeatSettings
