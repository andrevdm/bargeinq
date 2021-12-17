{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Registry
    ( mkApp
    ) where

import           Protolude

import qualified Components.AppCmp as CA
import qualified Impl.AppCmpIO as CA
import qualified Impl.QueueCmpIO as CQ

mkApp :: IO (CA.AppCmp IO)
mkApp = do
  let
    q = CQ.newQueueCmpIO @IO
  pure $ CA.newAppCmpIO @IO
