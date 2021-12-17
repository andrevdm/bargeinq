{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Components.DateCmp
    ( DateCmp(..)
    ) where

import qualified Data.Time as DT

data DateCmp m = DateCmp
  { dtGetDate :: m DT.UTCTime
  , dtGetTimeZone :: m DT.TimeZone
  , dtGetLocalDate :: m DT.LocalTime
  }
