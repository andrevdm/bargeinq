{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Impl.PsqlCmpIO
    ( newPsqlCmpIO
    , TracePg(..)
    ) where

import           Protolude hiding (catchJust, tryJust, throwIO)
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.Notification as Pg
import qualified Database.PostgreSQL.Simple.Transaction as Pg
import qualified Data.Pool as Po
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as TxtE
import qualified Data.Time as DT
import qualified System.TimeIt as Tim
import           Text.Printf (printf)
import           UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Async as UA
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
  => TracePg
  -> Text
  -> CL.LogCmp m
  -> m (CPg.PsqlCmp m)
newPsqlCmpIO traceFlag connStr lg = do
  poolQuery <- createPgConnPool 4 60 connStr
  poolNotify <- createPgConnPool 2 180 connStr

  pure CPg.PsqlCmp
    { CPg.pgQuery = \sql q name -> do
        when traceMessages $ CL.logDebug' lg "SQL:query" sql
        checkSlowQuery lg name =<<
          catchPsqlException lg name sql (withTimedPool poolQuery name $ \conn -> Pg.query conn sql q)

    , CPg.pgQuery_ = \sql name -> do
        when traceMessages $ CL.logDebug' lg "SQL:query_" sql
        checkSlowQuery lg name =<<
          catchPsqlException lg name sql (withTimedPool poolQuery name $ \conn -> Pg.query_ conn sql)

    , CPg.pgExecute = pgExec traceFlag lg poolQuery
    , CPg.pgExecute_ = pgExec_ traceFlag lg poolQuery

    , CPg.pgQuerySerializable = \sql q name -> do
        when traceMessages $ CL.logDebug' lg "SQL:querySerializable" sql
        checkSlowQuery lg name =<<
          catchPsqlException lg name sql (withTimedPool poolQuery name $ \conn -> Pg.withTransactionSerializable conn (Pg.query conn sql q))

    , CPg.pgGetNotification =
        liftIO . Po.withResource poolNotify $ Pg.getNotification

    , CPg.pgListenForNotifications = \chanName fn -> do
      --pgExec_ traceFlag lg poolNotify (Pg.Query . TxtE.encodeUtf8 $ "LISTEN " <> chanName) "listen" >>= \case
      pgExec traceFlag lg poolNotify "LISTEN ?" (Pg.Only chanName) "listen" >>= \case
        Left e -> UE.throwIO e
        Right _ -> pass

      void . UA.async . forever $ do
        n <- catchPsqlException lg "notify.fetch" "notify.fetch" $ Po.withResource poolNotify Pg.getNotification
        fn n
    }

  where
    traceMessages = traceFlag == TraceAll


pgExec
  :: forall m q.
     (MonadUnliftIO m, Pg.ToRow q)
  => TracePg
  -> CL.LogCmp m
  -> Po.Pool Pg.Connection
  -> Pg.Query
  -> q
  -> Text
  -> m (Either SomeException Int64)
pgExec t lg pool sql q name = do
  when (t == TraceAll) $ CL.logDebug' lg "SQL:execute" sql
  checkSlowQuery lg name =<<
    catchPsqlException lg name sql (withTimedPool pool name $ \conn -> Pg.execute conn sql q)


pgExec_
  :: (MonadUnliftIO m)
  => TracePg
  -> CL.LogCmp m
  -> Po.Pool Pg.Connection
  -> Pg.Query
  -> Text
  -> m (Either SomeException Int64)
pgExec_ t lg pool sql name = do
  when (t == TraceAll) $ CL.logDebug' lg "SQL:execute_" sql
  checkSlowQuery lg name =<<
    catchPsqlException lg name sql (withTimedPool pool name $ \conn -> Pg.execute_ conn sql)


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
  :: Po.Pool Pg.Connection
  -> Text
  -> (Pg.Connection -> IO a)
  -> IO (Double, a)
withTimedPool pool _name fn =
  Po.withResource pool (Tim.timeItT . fn)


catchPsqlException
  :: (MonadIO m)
  => CL.LogCmp m
  -> Text
  -> Pg.Query
  -> IO a
  -> m (Either SomeException a)
catchPsqlException lg name sql fn =
  liftIO (UE.tryJust Just fn) >>= \case
    Right r -> pure $ Right r
    Left e -> CL.logError' lg "psql error" (SqErr e name sql) >> pure (Left e)


data SqErr = SqErr
  { sqeError :: !SomeException
  , sqeName :: !Text
  , sqeSql :: !Pg.Query
  } deriving (Show)
