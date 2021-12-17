{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Impl.LogCmpIO
    ( newLogCmpIO
    ) where

import           Protolude hiding (catchJust, tryJust, log)
import qualified Data.Text.Lazy as TxtL
import qualified Text.Pretty.Simple as Pp

import qualified Components.DateCmp as CDt
import qualified Components.LogCmp as CL


newLogCmpIO
  :: forall m.
     (Monad m)
  => [CL.LogWriter m]
  -> CDt.DateCmp m
  -> CL.LogCmp m
newLogCmpIO logWriters dt =
  CL.LogBaseCmp
    { CL.logTest  = log CL.LevelTest  ""
    , CL.logDebug = log CL.LevelDebug ""
    , CL.logInfo  = log CL.LevelInfo  ""
    , CL.logWarn  = log CL.LevelWarn  ""
    , CL.logError = log CL.LevelError ""

    , CL.logTest'  = \t v -> log CL.LevelTest  t (ptnShow v)
    , CL.logDebug' = \t v -> log CL.LevelDebug t (ptnShow v)
    , CL.logInfo'  = \t v -> log CL.LevelInfo  t (ptnShow v)
    , CL.logWarn'  = \t v -> log CL.LevelWarn  t (ptnShow v)
    , CL.logError' = \t v -> log CL.LevelError t (ptnShow v)
    }
  where
    log lvl m s = do
      dt' <- CDt.dtGetLocalDate dt
      for_ logWriters $ \w -> CL.writeLog w dt' lvl m s


ptnShow :: (Show a) => a -> Text
ptnShow = TxtL.toStrict . Pp.pShowOpt (opts { Pp.outputOptionsColorOptions = Nothing })


opts :: Pp.OutputOptions
opts =
  Pp.OutputOptions
    { Pp.outputOptionsIndentAmount = 2
    , Pp.outputOptionsPageWidth = 180
    , Pp.outputOptionsCompact = True
    , Pp.outputOptionsCompactParens = False
    , Pp.outputOptionsInitialIndent = 0
    , Pp.outputOptionsStringStyle = Pp.EscapeNonPrintable
    , Pp.outputOptionsColorOptions = Just Pp.defaultColorOptionsDarkBg
    }
