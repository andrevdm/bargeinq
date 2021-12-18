{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Impl.PsqlCmpIO
    ( newPsqlCmpIO
    , TracePg(..)
    ) where

import           Verset hiding (catchJust, tryJust, throwIO)
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

    , CPg.pgListenForNotifications = runListenForNotifications
    }

  where
    traceMessages = traceFlag == TraceAll


runListenForNotifications :: Text -> (Either SomeException Pg.Notification -> m ()) -> m ()
runListenForNotifications chanName fn = do
  undefined
  --pgExec_ traceFlag lg poolNotify (Pg.Query . TxtE.encodeUtf8 $ "LISTEN " <> chanName) "listen" >>= \case
  ----pgExec traceFlag lg poolNotify "LISTEN ?" (Pg.Only chanName) "listen" >>= \case
  --  Left e -> UE.throwIO e
  --  Right _ -> pass

  --void . UA.async . forever $ do
  --  putText "psql.listen"
  --  n <- catchPsqlException lg "notify.fetch" "notify.fetch" $ Po.withResource poolNotify Pg.getNotification
  --  fn n


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
