{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module BargeInQueue
    ( mkBargeInQueue
    , version
    , versionTxt
    , writeMigrations
    ) where

import           Verset
import           Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TBMQueue as TBMQ
import           Control.Lens ((^.))
import qualified Data.Text as Txt
import qualified System.Directory as Dir
import           System.FilePath ((</>))
import           UnliftIO.Exception (throwString)
import qualified Data.Version as V

import qualified BargeInQueue.Core as C
import qualified BargeInQueue.Components.BargeInQueueCmp as CBq
import qualified BargeInQueue.Components.RepoCmp as CR
import qualified BargeInQueue.Impl.BargeInQueueCmpIO as CBq
import qualified BargeInQueue.Impl.DateCmpIO as CDt
import qualified BargeInQueue.Impl.EnvCmpIO as CE
import qualified BargeInQueue.Impl.LogCmpIO as CL
import qualified BargeInQueue.Impl.PsqlCmpIO as CPg
import qualified BargeInQueue.Impl.QueueCmpIO as CQ
import qualified BargeInQueue.Impl.RepoCmpPsql as CR
import qualified BargeInQueue.Impl.UuidCmpIO as CUu
import qualified BargeInQueue.Components.LogCmp as CL
import qualified Paths_bargeinq as Paths

version :: V.Version
version = Paths.version

versionTxt :: Text
versionTxt = Txt.pack $ V.showVersion Paths.version


mkBargeInQueue
  :: C.SystemId
  -> Text
  -> CPg.TracePg
  -> CL.LogLevel
  -> Text
  -> Maybe Int
  -> IO (CBq.BargeInQueueCmp IO)
mkBargeInQueue sysId connStr tracePg minLogLevel hostName hostMaxItems = do
  -- Logging
  prnQ <- atomically $ TBMQ.newTBMQueue 1000
  let termWriter = CL.createQueueLogWriter prnQ
  CL.startTerminalPrinter minLogLevel prnQ

  -- Create enough components so we can query the repo
  let dt = CDt.newDateCmpIO @IO
  let lg = CL.newLogCmpIO @IO [termWriter] dt
  pg <- CPg.newPsqlCmpIO tracePg connStr lg
  let repo = CR.newRepoCmpPsql pg dt

  -- Fetch the system config
  sysConfig <- CR.rpGetSystem repo sysId >>= \case
    Left e -> throwString $ Txt.unpack e
    Right Nothing -> throwString $ "BargeInQueue system does not exist: " <> show sysId
    Right (Just s) ->
      if s ^. C.sysId == sysId
        then pure s
        else throwString $ "Invalid SystemId returned. Expecting: " <> show sysId <> ", got: " <> show (s ^. C.sysId)

  -- Create the rest
  env <- CE.newEnvCmpIO sysConfig
  let uu = CUu.newUuidCmpIO @IO

  let hostMax = case hostMaxItems of
                  Nothing -> Nothing
                  Just i -> Just (hostName, i)

  q <- CQ.newQueueCmpIO @IO pg lg repo env dt sysConfig hostMax


  --ws <- CR.rpListUnqueuedUnblockedWorkItems repo sysId 10
  --print ws


  pure $ CBq.newBargeInQueueCmpIO q dt uu lg pg env repo



writeMigrations :: FilePath -> IO ()
writeMigrations dest = do
  Dir.createDirectoryIfMissing True dest

  dataDir <- Paths.getDataDir
  fs <- Dir.listDirectory dataDir
  for_ fs $ \p -> do
    Dir.copyFile (dataDir </> p) (dest </> p)


