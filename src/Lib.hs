{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import           Protolude
import qualified Data.UUID as UU

import qualified Queue as Q


testSysId :: Q.SystemId
testSysId = Q.SystemId $ UU.fromWords 918212935 1131432256 2699803656 3287151122

testWorkType :: Q.WorkTypeId
testWorkType = Q.WorkTypeId $ UU.fromWords 3523928252 3368372076 2491820724 2868489993


someFunc :: IO ()
someFunc = do
  putText "someFunc"


