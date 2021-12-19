{-# LANGUAGE NoImplicitPrelude #-}

module BargeInQueue.Components.UserCmp
    ( UserCmp(..)
    ) where

data UserCmp m = UserCmp
  { usrQueueStarting :: m ()
  }
