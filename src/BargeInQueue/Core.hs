{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module BargeInQueue.Core
    ( NewWorkItem(..)
    , wiId
    , wiName
    , wiSystemId
    , wiWorkerType
    , wiGroupId
    , wiDependsOnGroups
    , wiDependsOnWorkItem
    , wiOverrideIgnoreUntil
    , wiOverrideRetriesLeft

    , SystemConfig(..)
    , sysId
    , sysRequiresGlobalLock
    , sysPollPeriodSeconds
    , sysLockedUntil
    , sysLockedBy

    , WorkItemId(..)
    , SystemId(..)
    , WorkTypeId(..)
    , PendingWorkItems(..)
    , QueueWorkItems(..)
    ) where

import           Verset
import           Control.Lens (makeLenses)

newtype QueueHandle = QueueHandle Int deriving (Show, Eq)
newtype WorkItemId = WorkItemId UUID deriving (Show, Eq)
newtype SystemId = SystemId UUID deriving (Show, Eq)
newtype WorkTypeId = WorkTypeId UUID deriving (Show, Eq)
newtype PendingWorkItems = PendingWorkItems [NewWorkItem] deriving (Show, Eq)
newtype QueueWorkItems = QueueWorkItems [NewWorkItem] deriving (Show, Eq)


data NewWorkItem = NewWorkItem
  { _wiId :: !WorkItemId
  , _wiName :: !Text
  , _wiSystemId :: !SystemId
  , _wiWorkerType :: !WorkTypeId
  , _wiGroupId :: !(Maybe UUID)
  , _wiDependsOnGroups :: ![UUID]
  , _wiDependsOnWorkItem :: ![UUID]
  , _wiOverrideIgnoreUntil :: !(Maybe UTCTime)
  , _wiOverrideRetriesLeft :: !(Maybe Int)
  } deriving (Show, Eq)


data SystemConfig = SystemConfig
  { _sysId :: !UUID
  , _sysRequiresGlobalLock :: !Bool
  , _sysPollPeriodSeconds :: !Int
  , _sysLockedUntil :: !(Maybe UTCTime)
  , _sysLockedBy :: !(Maybe Text)
  } deriving (Show)


makeLenses ''NewWorkItem
makeLenses ''SystemConfig
