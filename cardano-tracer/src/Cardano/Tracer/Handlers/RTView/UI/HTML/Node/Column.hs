{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Node.Column
  ( addNodeColumn
  , deleteNodeColumn
  -- Description (first) column.
  , hideDescriptionColumnIfNeeded
  , showDescriptionColumnIfNeeded
  ) where

import           Control.Monad (unless, void, when)
import           Control.Monad.Extra (whenJustM)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (unpack)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

-- | For every connected node the new column should be added.
addNodeColumn
  :: UI.Window
  -> NodeId
  -> UI ()
addNodeColumn window (NodeId anId) = do
  addNodeCell "name"     [UI.span # set text "Node"]
  addNodeCell "version"  [UI.span # set text "-"]
  addNodeCell "protocol" [UI.span # set text "-"]
  addNodeCell "commit"   [UI.span # set text "-"]
  addNodeCell "platform" [UI.span # set text "-"]
  addNodeCell "peers"    [UI.span # set text "-"]
  addNodeCell "chain"    [UI.span # set text "-"]
  addNodeCell "errors"   [UI.span # set text "No errors"]
  addNodeCell "cpu"      [UI.span # set text "-"]
 where
  addNodeCell rowId cellContent =
    whenJustM (UI.getElementById window ("node-" <> rowId <> "-row")) $ \el ->
      void $ element el #+ [UI.th #. (unpack anId <> "__column_cell") #+ cellContent]
  
-- | The node was disconnected, so its column should be deleted.
deleteNodeColumn
  :: UI.Window
  -> NodeId
  -> UI ()
deleteNodeColumn window (NodeId anId) = do
  let className = anId <> "__column_cell"
  findByClassAndDo window className UI.delete

-- | If there are no connected nodes, description column should be hidden.
--   If there is at least one connected node, description column should be shown.
hideDescriptionColumnIfNeeded, showDescriptionColumnIfNeeded
  :: UI.Window -> Set NodeId -> UI ()
hideDescriptionColumnIfNeeded window connected =
  when (S.null connected) $
    findAndSet hideIt window "main-table"
showDescriptionColumnIfNeeded window connected =
  unless (S.null connected) $
    findByClassAndSet showInline window "description-column-cell"
