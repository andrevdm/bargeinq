{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Queue
    ( NewWorkItem(..)
    , SystemId(..)
    , WorkTypeId(..)
    , PendingWorkItems(..)
    , QueueWorkItems(..)
    , QueueHandle
    , queueWork
    , startQueue
    ) where

import           Protolude
import           Data.Time (UTCTime)
import           Data.UUID (UUID)


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


queueWork :: PendingWorkItems -> QueueWorkItems -> IO ()
queueWork (PendingWorkItems pis) (QueuedWorkItems qis) = do
  pass


startQueue :: SystemId -> IO QueueHandle
startQueue _sid = do
  undefined
