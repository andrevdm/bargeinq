{-# LANGUAGE NoImplicitPrelude #-}

module BargeInQueue.Core
    ( NewWorkItem(..)
    , WorkItemId(..)
    , SystemId(..)
    , WorkTypeId(..)
    , PendingWorkItems(..)
    , QueueWorkItems(..)
    , SystemConfig(..)
    ) where

import           Verset

newtype QueueHandle = QueueHandle Int deriving (Show, Eq)
newtype WorkItemId = WorkItemId UUID deriving (Show, Eq)
newtype SystemId = SystemId UUID deriving (Show, Eq)
newtype WorkTypeId = WorkTypeId UUID deriving (Show, Eq)
newtype PendingWorkItems = PendingWorkItems [NewWorkItem] deriving (Show, Eq)
newtype QueueWorkItems = QueueWorkItems [NewWorkItem] deriving (Show, Eq)


data NewWorkItem = NewWorkItem
  { wiId :: !WorkItemId
  , wiName :: !Text
  , wiSystemId :: !SystemId
  , wiWorkerType :: !WorkTypeId
  , wiGroupId :: !(Maybe UUID)
  , wiDependsOnGroups :: ![UUID]
  , wiDependsOnWorkItem :: ![UUID]
  , wiOverrideIgnoreUntil :: !(Maybe UTCTime)
  , wiOverrideRetriesLeft :: !(Maybe Int)
  } deriving (Show, Eq)


data SystemConfig = SystemConfig
  { scId :: !UUID
  , scRequiresGlobalLock :: !Bool
  , scPollPeriodSeconds :: !Int
  , scLockedUntil :: !(Maybe UTCTime)
  , scLockedBy :: !(Maybe Text)
  } deriving (Show)
