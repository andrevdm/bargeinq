{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module BargeInQueue.Impl.RepoCmpPsql
    ( newRepoCmpPsql
    ) where

import           Verset hiding (log)
import qualified Data.Text as Txt
import qualified Data.Time as DT

import qualified BargeInQueue.Core as C
import qualified BargeInQueue.Components.RepoCmp as CR


newRepoCmpPsql
  :: forall m.
     (Monad m)
  => CR.RepoCmp m
newRepoCmpPsql =
  CR.RepoCmp
    { CR.rpListSystems = undefined
    , CR.rpFetchSystem = undefined
    }
