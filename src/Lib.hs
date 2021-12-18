{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import           Protolude
import qualified Data.UUID as UU

import qualified Components.QueueCmp as CQ
import qualified Registry as Reg


testSysId :: CQ.SystemId
testSysId = CQ.SystemId $ UU.fromWords 918212935 1131432256 2699803656 3287151122

testWorkType :: CQ.WorkTypeId
testWorkType = CQ.WorkTypeId $ UU.fromWords 3523928252 3368372076 2491820724 2868489993


someFunc :: IO ()
someFunc = do
  q <- Reg.mkQueue
  putText "\n\n----------------------"
  _ <- CQ.qStartQueue q (CQ.SystemId UU.nil)

  putText "someFunc"


