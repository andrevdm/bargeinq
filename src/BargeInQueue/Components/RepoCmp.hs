{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module BargeInQueue.Components.RepoCmp
    ( RepoCmp(..)
    ) where

import           Verset

import qualified BargeInQueue.Core as C

data RepoCmp m = RepoCmp
  { rpListSystems :: !(m (Either Text [C.SystemConfig]))
  , rpGetSystem :: !(C.SystemId -> m (Either Text (Maybe C.SystemConfig)))
  }


