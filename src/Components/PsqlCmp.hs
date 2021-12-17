{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Components.PsqlCmp
    ( PsqlCmp(..)
    ) where

import           Protolude hiding (catchJust, tryJust)
import           Control.Exception.Safe (tryJust)
import qualified Data.Pool as Po
import qualified Data.Text as Txt
import qualified Database.PostgreSQL.Simple.FromRow as Pg
import qualified Database.PostgreSQL.Simple.Transaction as Pg
import qualified Database.PostgreSQL.Simple as Pg
import           Database.PostgreSQL.Simple.ToRow (ToRow (..))
import qualified System.TimeIt as Tim
import           Text.Printf (printf)

data PsqlCmp m =
  PsqlCmp
    { pgQuery :: forall q r. (Pg.ToRow q, Pg.FromRow r) => Po.Pool Pg.Connection -> Pg.Query -> q -> Text -> m (Either SomeException [r])
    , pgQuery_ :: forall r. (Pg.FromRow r) => Po.Pool Pg.Connection -> Pg.Query -> Text -> m (Either SomeException [r])
    , pgExecute :: forall q. (ToRow q) => Po.Pool Pg.Connection -> Pg.Query -> q -> Text -> m (Either SomeException Int64)
    , pgExecute_ :: Po.Pool Pg.Connection -> Pg.Query -> Text -> m (Either SomeException Int64)
    , pgQuerySerializable :: forall q r. (Pg.ToRow q, Pg.FromRow r) => Po.Pool Pg.Connection -> Pg.Query -> q -> Text -> m (Either SomeException [r])
    }
