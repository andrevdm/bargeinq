{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Components.LogCmp
    ( LogCmp(..)
    , LogLevel(..)
    , LogWriter(..)
    , LogEntry(..)
    , logLevelToId
    , logLevelToText
    ) where

import           Verset
import qualified Data.Time as DT


data LogLevel
  = LevelDebug
  | LevelInfo
  | LevelWarn
  | LevelError
  | LevelTest
  deriving (Show, Eq, Enum, Bounded)


logLevelToText :: LogLevel -> Text
logLevelToText LevelDebug = "debug"
logLevelToText LevelInfo = "info"
logLevelToText LevelWarn = "warn"
logLevelToText LevelError = "error"
logLevelToText LevelTest = "test"


logLevelToId :: LogLevel -> Int
logLevelToId LevelTest = 1
logLevelToId LevelDebug = 2
logLevelToId LevelInfo = 3
logLevelToId LevelWarn = 4
logLevelToId LevelError = 5


newtype LogWriter m =
  LogWriter
    { writeLog :: LogEntry -> m ()
    }

data LogEntry = LogEntry DT.LocalTime LogLevel Text Text deriving (Show, Eq)


data LogCmp m = LogBaseCmp
  { logTest :: Text -> m ()
  , logDebug :: Text -> m ()
  , logInfo :: Text -> m ()
  , logWarn :: Text -> m ()
  , logError :: Text -> m ()

  , logTest' :: forall s. (Show s) => Text -> s -> m ()
  , logDebug' :: forall s. (Show s) => Text -> s -> m ()
  , logInfo' :: forall s. (Show s) => Text -> s -> m ()
  , logWarn' :: forall s. (Show s) => Text -> s -> m ()
  , logError' :: forall s. (Show s) => Text -> s -> m ()
  }


