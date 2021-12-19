{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module BargeInQueue.Impl.EnvCmpIO
    ( newEnvCmpIO
    ) where

import           Verset
import           UnliftIO (MonadUnliftIO, throwString)
import           UnliftIO.STM (atomically, readTVarIO, newTVarIO, swapTVar, writeTVar)

import qualified BargeInQueue.Core as C
import qualified BargeInQueue.Components.EnvCmp as CE


newEnvCmpIO
  :: forall m.
     (MonadUnliftIO m)
  => C.SystemConfig
  -> m (CE.EnvCmp m)
newEnvCmpIO sys = do
  started' <- newTVarIO False
  usrCmp' <- newTVarIO Nothing

  pure CE.EnvCmp
    { CE.envIsStarted = readTVarIO started'
    , CE.envSystem = sys

    , CE.envSetStarted = \usrCmp -> do
        wasStarted <- atomically $ do
          ws <- swapTVar started' True
          unless ws $ writeTVar usrCmp' (Just usrCmp)
          pure ws

        when wasStarted $ throwString "Queue was already started"

    , CE.envDemandUser = do
        readTVarIO usrCmp' >>= \case
          Nothing -> throwString "The queue has not been started"
          Just u -> pure u
    }

