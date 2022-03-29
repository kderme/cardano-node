{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.DataPoints
  ( askNodeInfoIfNeeded
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forM, forM_, unless)
--import           Data.Map.Strict (Map)
--import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
--import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.State.DataPoint
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Types

-- | There is a different information the node can provide us by explicit request.
--   This is a structured data about internal state of the node (for example, its
--   basic information like version, protocol, commit hash, start time, etc).
--
--   Such a structured data is provided as a 'DataPoint'. When it's receved, it's
--   technically a lazy bytestring that is a result of 'ToJSON'-encoding on the
--   forwarder's side. Here we can decode it to particular Haskell type provided
--   by the node.
--
--   The common algorithm is this:
--
--   1. Ask for 'DataPoint' using its name and decode it to Haskell type.
--   2. Set the values of corresponding elements on the web page.
--   3. Save these values in 'displayedElements' to keep them for future.
--      It helps to avoid asking this data again after DOM re-rendering
--      (for example, after the user reloaded the web page).

-- | TMP, the type should be taken from 'cardano-node'.
type NodeInfo = Int

askNodeInfoIfNeeded
  :: DataPointRequestors
  -> Set NodeId
  -> DisplayedElements
  -> UI ()
askNodeInfoIfNeeded dpRequestors connectedNodes displayedElements =
  unless (S.null connectedNodes) $ do
    _dElements <- liftIO $ readTVarIO displayedElements
    nodesInfoValues <-
      forM (S.toList connectedNodes) $ \nodeId -> liftIO $
        checkSavedNodeInfo nodeId >>= \case
          Nothing ->
            getDataPointFromNode dpRequestors nodeId "name.of.node.info" >>= \case
              Nothing -> return Nothing -- The node didn't provide its info.
              Just (_nodeInfo :: NodeInfo) -> return Nothing
          Just _savedInfo ->
            return Nothing

    setElements nodesInfoValues
    -- saveElements
 where
  checkSavedNodeInfo _ = return Nothing

  setElements nodesInfoValues =
    forM_ nodesInfoValues $ \_nodeInfoValues -> do
      return ()
