{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Impl.LogCmpIO
    ( newLogCmpIO
    , createQueueLogWriter
    , startTerminalPrinter
    ) where

import           Verset hiding (log)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBMQueue (TBMQueue, writeTBMQueue, readTBMQueue)
import qualified Data.Colour.Names as Clr
import qualified Data.Text as Txt
import qualified Data.Time as DT
import qualified Data.Text.Lazy as TxtL
import qualified Text.Pretty.Simple as Pp
import qualified System.Console.ANSI as An

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
      for_ logWriters $ \w -> CL.writeLog w (CL.LogEntry dt' lvl m s)


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


createQueueLogWriter
  :: forall m.
    (MonadIO m)
  => TBMQueue CL.LogEntry
  -> CL.LogWriter m
createQueueLogWriter q =
  CL.LogWriter { CL.writeLog = liftIO . atomically . writeTBMQueue q}


startTerminalPrinter
  :: forall m.
     (MonadIO m)
  => TBMQueue CL.LogEntry
  -> m ()
startTerminalPrinter q = do
  void . liftIO . forkIO . forever $
    atomically (readTBMQueue q) >>= \case
      Nothing -> pass
      Just ((CL.LogEntry at level s d)) -> do
        An.setSGR $ case level of
                      CL.LevelError -> [An.SetColor An.Foreground An.Vivid An.Red]
                      CL.LevelWarn -> [An.SetColor An.Foreground An.Vivid An.Yellow]
                      CL.LevelInfo -> [An.SetColor An.Foreground An.Vivid An.Cyan]
                      CL.LevelDebug -> [An.SetRGBColor An.Foreground Clr.dimgrey]
                      CL.LevelTest -> [An.SetRGBColor An.Foreground Clr.pink]
        let dt = DT.formatTime DT.defaultTimeLocale "%Y-%m-%d %H:%M:%S.%q: " at
        putText $ Txt.pack dt <> s <> "  " <> d
        An.setSGR [An.Reset]
