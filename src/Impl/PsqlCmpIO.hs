{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Impl.PsqlCmpIO
    ( newPsqlCmpIO
    ) where

import           Protolude hiding (catchJust, tryJust)
import           Control.Exception.Safe (tryJust)
import qualified Data.Pool as Po
import qualified Data.Text as Txt
import qualified Database.PostgreSQL.Simple.Transaction as Pg
import qualified Database.PostgreSQL.Simple as Pg
import qualified System.TimeIt as Tim
import           Text.Printf (printf)

import qualified Components.PsqlCmp as CPg
import qualified Components.LogCmp as CL


newPsqlCmpIO
  :: forall m.
     (MonadIO m)
  => Bool
  -> CL.LogCmp m
  -> CPg.PsqlCmp m
newPsqlCmpIO traceMessages lg =
  CPg.PsqlCmp
    { CPg.pgQuery = \pool sql q name -> do
        when traceMessages $ CL.logDebug' lg "SQL:query" sql
        checkSlowQuery lg name =<<
          catchPsqlException lg name sql (withTimedPool pool name $ \conn -> Pg.query conn sql q)

    , CPg.pgQuery_ = \pool sql name -> do
        when traceMessages $ CL.logDebug' lg "SQL:query_" sql
        checkSlowQuery lg name =<<
          catchPsqlException lg name sql (withTimedPool pool name $ \conn -> Pg.query_ conn sql)

    , CPg.pgExecute = \pool sql q name -> do
        when traceMessages $ CL.logDebug' lg "SQL:execute" sql
        checkSlowQuery lg name =<<
          catchPsqlException lg name sql (withTimedPool pool name $ \conn -> Pg.execute conn sql q)

    , CPg.pgExecute_ = \pool sql name -> do
        when traceMessages $ CL.logDebug' lg "SQL:execute_" sql
        checkSlowQuery lg name =<<
          catchPsqlException lg name sql (withTimedPool pool name $ \conn -> Pg.execute_ conn sql)

    , CPg.pgQuerySerializable = \pool sql q name -> do
        when traceMessages $ CL.logDebug' lg "SQL:querySerializable" sql
        checkSlowQuery lg name =<<
          catchPsqlException lg name sql (withTimedPool pool name $ \conn -> Pg.withTransactionSerializable conn (Pg.query conn sql q))
    }


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
  -> IO (Double, a)
  -> m (Either SomeException (Double, a))
catchPsqlException lg name sql fn =
  liftIO (tryJust Just fn) >>= \case
    Right r -> pure $ Right r
    Left e -> CL.logError' lg "psql error" (SqErr e name sql) >> pure (Left e)


data SqErr = SqErr
  { sqeError :: !SomeException
  , sqeName :: !Text
  , sqeSql :: !Pg.Query
  } deriving (Show)
