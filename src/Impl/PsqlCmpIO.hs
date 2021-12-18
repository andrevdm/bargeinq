{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Impl.PsqlCmpIO
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
import           UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Async as UA
import qualified UnliftIO.Concurrent as UC
import qualified UnliftIO.Exception as UE

import qualified Components.PsqlCmp as CPg
import qualified Components.LogCmp as CL

data TracePg
  = TraceAll
  | TraceNone
  deriving (Show, Eq)


createPgConnPool
  :: (MonadUnliftIO m)
  => Int
  -> DT.NominalDiffTime
  -> Text
  -> m (Po.Pool Pg.Connection)
createPgConnPool maxConns keepOpenSecs cstr =
  liftIO $ Po.createPool
    (Pg.connectPostgreSQL $ TxtE.encodeUtf8 cstr) -- how to create a new connection
    Pg.close                                      -- how to close a connection
    1                                             -- number of stripes (sub-pools)
    keepOpenSecs                                  -- seconds to keep a connection open
    maxConns                                      -- max number of connections


newPsqlCmpIO
  :: forall m.
     (MonadUnliftIO m)
  => TracePg -> Text
  -> CL.LogCmp m
  -> m (CPg.PsqlCmp m)
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

    , CPg.pgListenForNotifications = runListenForNotifications lg connStr
    }

  where
    traceMessages = traceFlag == TraceAll


runListenForNotifications
  :: forall m.
     (MonadUnliftIO m)
  => CL.LogCmp m
  -> Text
  -> Text
  -> (Pg.Notification -> m ())
  -> m ()
runListenForNotifications lg connStr chanName fn = do
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
          CL.logDebug lg "LISTEN: running LISTEN on new connection"
          _ <- liftIO $ Pg.execute_ conn (Pg.Query . TxtE.encodeUtf8 $ "LISTEN " <> chanName) -- run LISTEN command for the new connection
          forever $ runListen conn
        )

    silent f =
      UE.catch f (\(_ :: SomeException) -> pass)

    runListen conn = do
      n <- liftIO $ Pg.getNotification conn
      fn n


checkSlowQuery
  :: forall a m.
     (Monad m)
  => CL.LogCmp m
  -> Text
  -> Either SomeException (Double, a)
  -> m (Either SomeException a)
checkSlowQuery lg name res =
  case res of
    Left e ->
      pure . Left $ e

    Right (time, r) -> do
      when (time > 0.9) $ CL.logWarn' lg ("SQL:slow query - " <> name) (Txt.pack $ printf "%.4f" time)
      pure . Right $ r


withTimedPool
  :: forall a m.
     (MonadUnliftIO m)
  => Po.Pool Pg.Connection
  -> Text
  -> (Pg.Connection -> IO a)
  -> m (Double, a)
withTimedPool pool _name fn =
  liftIO $ Po.withResource pool (Tim.timeItT . fn)


catchPsqlException
  :: (MonadUnliftIO m)
  => CL.LogCmp m
  -> Text
  -> Pg.Query
  -> m a
  -> m (Either SomeException a)
catchPsqlException lg name sql fn =
  UE.tryJust Just fn >>= \case
    Right r -> pure $ Right r
    Left e -> CL.logError' lg "psql error" (SqErr e name sql) >> pure (Left e)


data SqErr = SqErr
  { sqeError :: !SomeException
  , sqeName :: !Text
  , sqeSql :: !Pg.Query
  } deriving (Show)
