{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module BargeInQueue.Components.PsqlCmp
    ( PsqlCmp(..)
    , ChanName(..)
    ) where

import           Verset
import qualified Database.PostgreSQL.Simple.FromRow as Pg
import qualified Database.PostgreSQL.Simple as Pg
import           Database.PostgreSQL.Simple.ToRow (ToRow (..))
import qualified Database.PostgreSQL.Simple.Notification as Pg

newtype ChanName = ChanName Text deriving (Show, Eq, Ord)

data PsqlCmp m =
  PsqlCmp
    { pgQuery :: forall q r. (Pg.ToRow q, Pg.FromRow r) => Pg.Query -> q -> Text -> m (Either SomeException [r])
    , pgQuery_ :: forall r. (Pg.FromRow r) => Pg.Query -> Text -> m (Either SomeException [r])
    , pgExecute :: forall q. (ToRow q) => Pg.Query -> q -> Text -> m (Either SomeException Int64)
    , pgExecute_ :: Pg.Query -> Text -> m (Either SomeException Int64)
    , pgQuerySerializable :: forall q r. (Pg.ToRow q, Pg.FromRow r) => Pg.Query -> q -> Text -> m (Either SomeException [r])
    , pgListenForNotifications :: ChanName -> (Pg.Notification -> m ()) -> m ()
    , pgNotify :: ChanName -> Text -> m ()
    }
