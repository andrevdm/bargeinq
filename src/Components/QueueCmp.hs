{-# LANGUAGE NoImplicitPrelude #-}

module Components.QueueCmp
    ( NewWorkItem(..)
    , SystemId(..)
    , WorkTypeId(..)
    , PendingWorkItems(..)
    , QueueWorkItems(..)
    , QueueCmp(..)
    ) where

import           Protolude
import           Data.Time (UTCTime)
import           Data.UUID (UUID)
import qualified UnliftIO.Async as UA

newtype QueueHandle = QueueHandle Int deriving (Show, Eq)
newtype SystemId = SystemId UUID deriving (Show, Eq)
newtype WorkTypeId = WorkTypeId UUID deriving (Show, Eq)
newtype PendingWorkItems = PendingWorkItems [NewWorkItem] deriving (Show, Eq)
newtype QueueWorkItems = QueuedWorkItems [NewWorkItem] deriving (Show, Eq)


data NewWorkItem = NewWorkItem
  { wiId :: !UUID
  , wiSystemId :: !UUID
  , wiWorkerType :: !UUID
  , wiGroupId :: !UUID
  , wiDependsOnGroups :: ![UUID]
  , wiDependsOnWorkItem :: ![UUID]
  , wiOverrideIgnoreUntil :: !(Maybe UTCTime)
  , wiOverrideRetriesLeft :: !(Maybe Int)
  } deriving (Show, Eq)



data QueueCmp m = QueueCmp
  { qStartQueue :: !(SystemId -> m (UA.Async ()))
  , qQueueWork :: !(PendingWorkItems -> QueueWorkItems -> m ())
  }

