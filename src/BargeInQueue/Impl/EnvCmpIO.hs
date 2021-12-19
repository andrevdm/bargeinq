{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module BargeInQueue.Impl.EnvCmpIO
    ( newEnvCmpIO
    ) where

import           Verset
import           UnliftIO (MonadUnliftIO, throwString)
import           UnliftIO.STM (atomically, readTVarIO, newTVarIO, swapTVar)

import qualified BargeInQueue.Core as C
import qualified BargeInQueue.Components.EnvCmp as CE


newEnvCmpIO
  :: forall m.
     (MonadUnliftIO m)
  => C.SystemConfig
  -> m (CE.EnvCmp m)
newEnvCmpIO sys = do
  started <- newTVarIO False

  pure CE.EnvCmp
    { CE.envIsStarted = readTVarIO started
    , CE.envSystem = sys

    , CE.envSetStarted = do
        wasStarted <- atomically $ do swapTVar started True
        when wasStarted $ throwString ""
    }
