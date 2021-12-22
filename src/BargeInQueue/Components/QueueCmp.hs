{-# LANGUAGE NoImplicitPrelude #-}

module BargeInQueue.Components.QueueCmp
    ( QueueCmp(..)
    ) where

data QueueCmp m = QueueCmp
  { qStartQueue :: !(m ())
  , qCheckUnblocked :: !(m ())
  , qTriggerPoll :: !(m ())
  }

