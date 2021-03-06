{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module BargeInQueue.Impl.DateCmpIO
    ( newDateCmpIO
    ) where

import           Verset
import qualified Data.Time as DT

import qualified BargeInQueue.Components.DateCmp as CDt


newDateCmpIO :: (MonadIO m) => CDt.DateCmp m
newDateCmpIO =
  CDt.DateCmp
  { CDt.dtGetDate = liftIO DT.getCurrentTime
  , CDt.dtGetTimeZone = liftIO DT.getCurrentTimeZone
  , CDt.dtGetLocalDate = liftIO $ DT.utcToLocalTime <$> DT.getCurrentTimeZone <*> DT.getCurrentTime
  }


