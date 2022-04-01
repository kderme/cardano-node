{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.UI.Updater
  ( updateUI
  , updateMetricsUI
  ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (readTVar, readTVarIO)
import           Control.Monad (forM_, unless, when)
import qualified Data.Map.Strict as M
import           Data.Set (Set, (\\))
import qualified Data.Set as S
import           Data.Text (unpack)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.UI.DataPoints
import           Cardano.Tracer.Handlers.RTView.UI.HTML.Node.Column
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

updateUI
  :: UI.Window
  -> ConnectedNodes
  -> DisplayedNodes
  -> DisplayedElements
  -> DataPointRequestors
  -> SavedTraceObjects
  -> PageReloadedFlag
  -> UI ()
updateUI window connectedNodes displayedNodes displayedElements
         dpRequestors savedTO reloadFlag = do
  (connected, displayed, afterReload) <- liftIO . atomically $ (,,)
    <$> readTVar connectedNodes
    <*> readTVar displayedNodes
    <*> readTVar reloadFlag
  if afterReload
    then do
      -- Ok, web-page was reload (i.e. it's the first update after DOM-rendering),
      -- so displayed state should be restored immediately.
      addColumnsForConnected window connected
      checkNoNodesState window connected
      askNodeInfoIfNeeded dpRequestors connected displayedElements
      liftIO $ do
        updateDisplayedNodes displayedNodes connected
        updateDisplayedElements displayedElements connected
      liftIO $ pageWasNotReload reloadFlag
    else do
      -- Check connected/disconnected nodes.
      when (connected /= displayed) $ do
        let disconnected   = displayed \\ connected -- In 'displayed' but not in 'connected'.
            newlyConnected = connected \\ displayed -- In 'connected' but not in 'displayed'.
        deleteColumnsForDisconnected window connected disconnected
        addColumnsForConnected window newlyConnected
        checkNoNodesState window connected
        askNodeInfoIfNeeded dpRequestors newlyConnected displayedElements
        liftIO $ do
          updateDisplayedNodes displayedNodes connected
          updateDisplayedElements displayedElements connected
  -- Check if we have to update elements on the page using received 'TraceObject's.
  checkAcceptedTraceObjects window displayedElements savedTO

addColumnsForConnected
  :: UI.Window
  -> Set NodeId
  -> UI ()
addColumnsForConnected window connected = do
  showDescriptionColumnIfNeeded window connected
  forM_ connected $ addNodeColumn window

deleteColumnsForDisconnected
  :: UI.Window
  -> Set NodeId
  -> Set NodeId
  -> UI ()
deleteColumnsForDisconnected window connected disconnected = do
  forM_ disconnected $ deleteNodeColumn window
  hideDescriptionColumnIfNeeded window connected

checkNoNodesState :: UI.Window -> Set NodeId -> UI ()
checkNoNodesState window connected =
  if S.null connected
    then findAndShow window "no-nodes"
    else findAndHide window "no-nodes"

checkAcceptedTraceObjects
  :: UI.Window
  -> DisplayedElements
  -> SavedTraceObjects
  -> UI ()
checkAcceptedTraceObjects window displayedElements savedTO = do
  savedTraceObjects <- liftIO $ readTVarIO savedTO
  forM_ (M.toList savedTraceObjects) $ \(nodeId, savedForNode) ->
    forM_ (M.toList savedForNode) $ \(namespace, toValue) ->
      updateElementsIfNeeded window displayedElements nodeId namespace toValue

updateElementsIfNeeded
  :: UI.Window
  -> DisplayedElements
  -> NodeId
  -> Namespace
  -> TraceObjectTValue
  -> UI ()
updateElementsIfNeeded window displayedElements nodeId namespace toValue = do
  case namespace of
    "density" -> updateElement
    "slotNum" -> return ()
    "blockNum" -> return ()
    "slotInEpoch" -> return ()
    "epoch" -> return ()
    "forks" -> return ()
    "txsInMempool"  -> return ()
    "mempoolBytes"  -> return ()
    "txsProcessedNum"  -> return ()
    "blocksForgedNum"  -> return ()
    "nodeCannotForge"  -> return ()
    "nodeIsLeaderNum"  -> return ()
    "slotsMissedNum" -> return ()
    "operationalCertificateStartKESPeriod"  -> return ()
    "operationalCertificateExpiryKESPeriod"  -> return ()
    "currentKESPeriod"  -> return ()
    "remainingKESPeriods" -> return ()
    _ -> return ()
 where
  updateElement = do
    let elId = ""
        elValue = toValue
    liftIO (getDisplayedValue displayedElements nodeId elId) >>= \case
      Nothing ->
        -- There is no displayed value for this element yet.
        setAndSave elId elValue
      Just displayedValue ->
        -- There is a value that already displayed, check if it changed.
        unless (elValue == displayedValue) $
          setAndSave elId elValue
   where
     setAndSave elId elValue = do
      findAndSet (set text $ unpack elValue) window elId
      liftIO $ saveDisplayedValue displayedElements nodeId elId elValue

updateMetricsUI
  :: UI.Window
  -> AcceptedMetrics
  -> UI ()
updateMetricsUI _window acceptedMetrics = do
  allMetrics <- liftIO $ readTVarIO acceptedMetrics
  forM_ (M.toList allMetrics) $ \(_nodeId, (ekgStore, _)) -> do
    metrics <- liftIO $ getListOfMetrics ekgStore
    forM_ metrics $ \(_metricName, _metricValue) ->
      return ()
{-
      updateMetricsElements window nodeId metricName metricValue

updateMetricsElements
  :: UI.Window
  -> NodeId
  -> MetricName
  -> MetricValue
  -> UI ()
updateMetricsElements window nodeId metricName metricValue =
  case metricName of
    "Stat.Cputicks" -> return ()
    "Mem.Resident" -> return ()
    "RTS.GcLiveBytes" -> return ()
    "RTS.GcMajorNum" -> return ()
    "RTS.GcMinorNum" -> return ()
    "RTS.Gcticks" -> return ()
    "RTS.Mutticks" -> return ()
    "Stat.Threads" -> return ()
    _ -> return ()
-}
