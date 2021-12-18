{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Lib
    ( run
    ) where

import           Protolude
import qualified Data.UUID as UU
import qualified Data.UUID.V4 as UU

import qualified Components.BargeInQueueCmp as CBq
import qualified Components.QueueCmp as CQ
import qualified Impl.PsqlCmpIO as CPg
import qualified BargeInQueue as Bq


testSysId :: CQ.SystemId
testSysId = CQ.SystemId $ UU.fromWords 918212935 1131432256 2699803656 3287151122

testWorkType :: CQ.WorkTypeId
testWorkType = CQ.WorkTypeId $ UU.fromWords 3523928252 3368372076 2491820724 2868489993


run :: IO ()
run = do
  bq <- Bq.mkBargeInQueue
          (CQ.SystemId UU.nil)
          "postgres://bargeinq@127.0.0.1:5432/bargeinq?sslmode=disable&options=--search_path%3dpublic"
          CPg.TraceAll

  pend1Id <- CQ.WorkItemId <$> UU.nextRandom
  active1Id <- CQ.WorkItemId <$> UU.nextRandom

  let toPending = CQ.PendingWorkItems
       [ CQ.NewWorkItem
         { CQ.wiId = pend1Id
         , CQ.wiName = "pending1"
         , CQ.wiSystemId = testSysId
         , CQ.wiWorkerType = testWorkType
         , CQ.wiGroupId = Nothing
         , CQ.wiDependsOnGroups = []
         , CQ.wiDependsOnWorkItem = []
         , CQ.wiOverrideIgnoreUntil = Nothing
         , CQ.wiOverrideRetriesLeft = Nothing
         }
       ]

  let toActive = CQ.QueueWorkItems
       [ CQ.NewWorkItem
         { CQ.wiId = active1Id
         , CQ.wiName = "active1"
         , CQ.wiSystemId = testSysId
         , CQ.wiWorkerType = testWorkType
         , CQ.wiGroupId = Nothing
         , CQ.wiDependsOnGroups = []
         , CQ.wiDependsOnWorkItem = []
         , CQ.wiOverrideIgnoreUntil = Nothing
         , CQ.wiOverrideRetriesLeft = Nothing
         }
       ]

  CBq.bqQueueWork bq toPending toActive

  putText "\n\n----------------------"
  loop

  where
    loop = do
      getLine >>= \case
        "q" -> pass
        _ -> loop


