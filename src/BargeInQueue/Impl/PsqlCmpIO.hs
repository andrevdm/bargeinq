{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BargeInQueue.Impl.PsqlCmpIO
    ( newPsqlCmpIO
    , TracePg(..)
    ) where

import           Verset
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.Notification as Pg
import qualified Database.PostgreSQL.Simple.Types as Pg
import qualified Database.PostgreSQL.Simple.Transaction as Pg
import qualified Data.Pool as Po
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as TxtE
import qualified Data.Time as DT
import qualified System.TimeIt as Tim
import           Text.Printf (printf)
import qualified UnliftIO.Async as UA
import qualified UnliftIO.Concurrent as UC
import qualified UnliftIO.Exception as UE

import qualified BargeInQueue.Components.PsqlCmp as CPg
import qualified BargeInQueue.Components.LogCmp as CL

data TracePg
  = TraceStandard
  | TraceAll
  | TraceNone
  deriving (Show, Eq)


createPgConnPool
  :: Int
  -> DT.NominalDiffTime
  -> Text
  -> IO (Po.Pool Pg.Connection)
createPgConnPool maxConns keepOpenSecs cstr =
  liftIO $ Po.createPool
    (Pg.connectPostgreSQL $ TxtE.encodeUtf8 cstr) -- how to create a new connection
    Pg.close                                      -- how to close a connection
    1                                             -- number of stripes (sub-pools)
    keepOpenSecs                                  -- seconds to keep a connection open
    maxConns                                      -- max number of connections


newPsqlCmpIO
  :: TracePg
  -> Text
  -> CL.LogCmp IO
  -> IO (CPg.PsqlCmp IO)
newPsqlCmpIO traceFlag connStr lg = do
  poolQuery <- createPgConnPool 4 60 connStr

  pure CPg.PsqlCmp
    { CPg.pgQuery = \sql q name -> do
        when traceMessages $ CL.logDebug' lg "SQL:query" sql
        checkSlowQuery lg name =<<
          catchPsqlException lg name sql (withTimedPool poolQuery name $ \conn -> Pg.query conn sql q)

    , CPg.pgQuery_ = \sql name -> do
        when traceMessages $ CL.logDebug' lg "SQL:query_" sql
        checkSlowQuery lg name =<<
          catchPsqlException lg name sql (withTimedPool poolQuery name $ \conn -> Pg.query_ conn sql)

    , CPg.pgExecute = \sql q name -> do
        when traceMessages $ CL.logDebug' lg "SQL:execute" sql
        checkSlowQuery lg name =<<
          catchPsqlException lg name sql (withTimedPool poolQuery name $ \conn -> Pg.execute conn sql q)

    , CPg.pgExecute_ = \sql name -> do
        when traceMessages $ CL.logDebug' lg "SQL:execute_" sql
        checkSlowQuery lg name =<<
          catchPsqlException lg name sql (withTimedPool poolQuery name $ \conn -> Pg.execute_ conn sql)

    , CPg.pgQuerySerializable = \sql q name -> do
        when traceMessages $ CL.logDebug' lg "SQL:querySerializable" sql
        checkSlowQuery lg name =<<
          catchPsqlException lg name sql (withTimedPool poolQuery name $ \conn -> Pg.withTransactionSerializable conn (Pg.query conn sql q))

    , CPg.pgListenForNotifications = runListenForNotifications lg connStr traceFlag
    , CPg.pgNotify = runNotify lg poolQuery traceFlag

    , CPg.pgWithTransaction = \name fn' ->
        withTimedPool poolQuery name $ \conn -> Pg.withTransaction conn (fn' $ Pg.rollback conn)
    }

  where
    traceMessages = traceFlag /= TraceNone


runNotify
  :: CL.LogCmp IO
  -> Po.Pool Pg.Connection
  -> TracePg
  -> CPg.ChanName
  -> Text
  -> IO ()
runNotify lg pool traceFlag (CPg.ChanName chanName) s = do
  when (traceFlag == TraceAll) $ CL.logTest' lg "SQL:notify" (chanName <> ": " <> s)
  let name = "notify"
  let sql = Pg.Query . TxtE.encodeUtf8 $ "select null from pg_notify(?, ?)"
  _ <- checkSlowQuery lg name =<<
    catchPsqlException lg name sql (withTimedPool pool name $ \conn -> do
      _ :: [Pg.Only Pg.Null] <- Pg.query conn sql (chanName, s) -- run NOTIFY
      pass)
  pass


runListenForNotifications
  :: CL.LogCmp IO
  -> Text
  -> TracePg
  -> CPg.ChanName
  -> (Pg.Notification -> IO ())
  -> IO ()
runListenForNotifications lg connStr traceFlag (CPg.ChanName chanName) fn = do
  void . UA.async $ retry (0 :: Int)

  where
    backoff = [1, 2, 10, 30 :: Int]
    retry r = do
      when (r > 0) $ do
        let b = fromMaybe 60 (atMay backoff r)
        CL.logDebug lg $ "LISTEN: backoff pause seconds = " <> show b
        UC.threadDelay (1000000 * b)

      void $ UE.catch
        runConnect
        (\(e :: SomeException) -> do
          CL.logWarn' lg "Exception listening for notifications" e
          retry $ r + 1
        )

    runConnect = do
      UE.bracket
        (liftIO . Pg.connectPostgreSQL $ TxtE.encodeUtf8 connStr)
        (silent . liftIO . Pg.close)
        (\conn -> do
          let sql = "select null from fn_bq_listen(?)"
          when (traceFlag == TraceAll) $ CL.logDebug' lg "LISTEN: running LISTEN on new connection" (sql, chanName)
          _ :: [Pg.Only Pg.Null] <- liftIO $ Pg.query conn sql (Pg.Only chanName) -- run LISTEN (fn_list just makes the chan name a param)
          forever $ runListen conn
        )

    silent f =
      UE.catch f (\(_ :: SomeException) -> pass)

    runListen conn = do
      n <- liftIO $ Pg.getNotification conn
      fn n


checkSlowQuery
  :: CL.LogCmp IO
  -> Text
  -> Either SomeException (Double, a)
  -> IO (Either SomeException a)
checkSlowQuery lg name res =
  case res of
    Left e ->
      pure . Left $ e

    Right (time, r) -> do
      when (time > 0.9) $ CL.logWarn' lg ("SQL:slow query - " <> name) (Txt.pack $ printf "%.4f" time)
      pure . Right $ r


withTimedPool
  :: Po.Pool Pg.Connection
  -> Text
  -> (Pg.Connection -> IO a)
  -> IO (Double, a)
withTimedPool pool _name fn =
  liftIO $ Po.withResource pool (Tim.timeItT . fn)


catchPsqlException
  :: CL.LogCmp IO
  -> Text
  -> Pg.Query
  -> IO a
  -> IO (Either SomeException a)
catchPsqlException lg name sql fn =
  UE.tryJust Just fn >>= \case
    Right r -> pure $ Right r
    Left e -> CL.logError' lg "psql error" (SqErr e name sql) >> pure (Left e)


data SqErr = SqErr
  { sqeError :: !SomeException
  , sqeName :: !Text
  , sqeSql :: !Pg.Query
  } deriving (Show)
