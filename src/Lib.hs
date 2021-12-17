{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import           Protolude

someFunc :: IO ()
someFunc = putText "someFunc"


data WorkItem = WorkItem
  { wiId :: !Int

  }
