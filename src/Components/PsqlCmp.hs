{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Components.PsqlCmp
    ( PsqlCmp(..)
    ) where

import           Protolude hiding (catchJust, tryJust)
import qualified Database.PostgreSQL.Simple.FromRow as Pg
import qualified Database.PostgreSQL.Simple as Pg
import           Database.PostgreSQL.Simple.ToRow (ToRow (..))

data PsqlCmp m =
  PsqlCmp
    { pgQuery :: forall q r. (Pg.ToRow q, Pg.FromRow r) => Pg.Query -> q -> Text -> m (Either SomeException [r])
    , pgQuery_ :: forall r. (Pg.FromRow r) => Pg.Query -> Text -> m (Either SomeException [r])
    , pgExecute :: forall q. (ToRow q) => Pg.Query -> q -> Text -> m (Either SomeException Int64)
    , pgExecute_ :: Pg.Query -> Text -> m (Either SomeException Int64)
    , pgQuerySerializable :: forall q r. (Pg.ToRow q, Pg.FromRow r) => Pg.Query -> q -> Text -> m (Either SomeException [r])
    }
