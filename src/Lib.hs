{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Lib
    ( run
    ) where

import           Verset
import qualified Data.UUID as UU
import qualified Data.UUID.V4 as UU
import qualified System.IO as IO

import qualified BargeInQueue.Core as C
import qualified BargeInQueue.Components.BargeInQueueCmp as CBq
import qualified BargeInQueue.Impl.PsqlCmpIO as CPg
import qualified BargeInQueue as Bq


testSysId :: C.SystemId
testSysId = C.SystemId $ UU.fromWords 918212935 1131432256 2699803656 3287151122

testWorkType :: C.WorkTypeId
testWorkType = C.WorkTypeId $ UU.fromWords 3523928252 3368372076 2491820724 2868489993


run :: IO ()
run = do
  bq <- Bq.mkBargeInQueue
          testSysId
          "postgres://bargeinq@127.0.0.1:5432/bargeinq?sslmode=disable&options=--search_path%3dpublic"
          CPg.TraceStandard

  pend1Id <- C.WorkItemId <$> UU.nextRandom
  active1Id <- C.WorkItemId <$> UU.nextRandom

  let toPending = C.PendingWorkItems
       [ C.NewWorkItem
         { C.wiId = pend1Id
         , C.wiName = "pending1"
         , C.wiSystemId = testSysId
         , C.wiWorkerType = testWorkType
         , C.wiGroupId = Nothing
         , C.wiDependsOnGroups = []
         , C.wiDependsOnWorkItem = []
         , C.wiOverrideIgnoreUntil = Nothing
         , C.wiOverrideRetriesLeft = Nothing
         }
       ]

  let toActive = C.QueueWorkItems
       [ C.NewWorkItem
         { C.wiId = active1Id
         , C.wiName = "active1"
         , C.wiSystemId = testSysId
         , C.wiWorkerType = testWorkType
         , C.wiGroupId = Nothing
         , C.wiDependsOnGroups = []
         , C.wiDependsOnWorkItem = []
         , C.wiOverrideIgnoreUntil = Nothing
         , C.wiOverrideRetriesLeft = Nothing
         }
       ]

  CBq.bqQueueWork bq toPending toActive

  putText "\n\n----------------------"
  loop

  where
    loop = do
      IO.getLine >>= \case
        "q" -> pass
        _ -> loop


