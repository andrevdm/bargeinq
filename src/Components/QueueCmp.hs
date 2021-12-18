{-# LANGUAGE NoImplicitPrelude #-}

module Components.QueueCmp
    ( NewWorkItem(..)
    , WorkItemId(..)
    , SystemId(..)
    , WorkTypeId(..)
    , PendingWorkItems(..)
    , QueueWorkItems(..)
    , QueueCmp(..)
    ) where

import           Verset
import           Data.Time (UTCTime)
import           Data.UUID (UUID)

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



data QueueCmp m = QueueCmp
  { qStartQueue :: !(m ())
  , qQueueWork :: !(PendingWorkItems -> QueueWorkItems -> m ())
  }

