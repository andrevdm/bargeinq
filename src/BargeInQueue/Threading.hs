{-# LANGUAGE NoImplicitPrelude #-}

module BargeInQueue.Threading
    ( Gate(..)
    , newOpenGate
    , newClosedGate
    , closeGate
    , openGate
    , waitForOpenGateAndClose
    , waitForOpenGate
    , isOpenGate
    ) where

import           Verset
import           UnliftIO.STM (atomically)
import qualified UnliftIO.STM as US
import           UnliftIO (MonadUnliftIO)


---------------------------------------------------------------------------------------------------------------------
-- Gate
---------------------------------------------------------------------------------------------------------------------
-- | Simple Gate synchronisation type
-- A gate can be opened or closed
-- If open any number of threads can continue
-- If closed nothing can continue
newtype Gate = Gate (US.TMVar ())

newOpenGate :: (MonadUnliftIO m) => m Gate
newOpenGate = Gate <$> US.newTMVarIO ()

newClosedGate :: (MonadUnliftIO m) => m Gate
newClosedGate = Gate <$> US.newEmptyTMVarIO

closeGate :: (MonadUnliftIO m) => Gate -> m ()
closeGate (Gate g) = void . atomically $ US.tryTakeTMVar g

openGate :: (MonadUnliftIO m) => Gate -> m ()
openGate (Gate g) = void . atomically $ US.tryPutTMVar g ()

waitForOpenGateAndClose :: (MonadUnliftIO m) => Gate -> m ()
waitForOpenGateAndClose (Gate g) = void . atomically $ US.takeTMVar g

waitForOpenGate :: (MonadUnliftIO m) => Gate -> m ()
waitForOpenGate (Gate g) = void . atomically $ US.readTMVar g

isOpenGate :: (MonadUnliftIO m) => Gate -> m Bool
isOpenGate (Gate g) = atomically $ not <$> US.isEmptyTMVar g

---------------------------------------------------------------------------------------------------------------------
