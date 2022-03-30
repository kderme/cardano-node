module Cardano.Tracer.Handlers.RTView.UI.HTML.PageBody
  ( mkPageBody
  ) where

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.HTML.OwnInfo (mkOwnInfo)
import           Cardano.Tracer.Handlers.RTView.UI.Utils

mkPageBody :: UI.Window -> UI Element
mkPageBody window =
  UI.getBody window #+
    [ UI.div ## "preloader" #. "pageloader is-active" #+
        [ UI.span #. "title" # set text "Just a second..."
        ]
    , topNavigation
    , UI.div ## "no-nodes" #. "container is-max-widescreen has-text-centered" #+
        [ image "rt-view-no-nodes-icon" noNodesSVG
        , UI.p #. "rt-view-no-nodes-message" #+
            [ string "There are no connected nodes. Yet."
            ]
        ]
    , UI.mkElement "section" #. "section" #+
        [ UI.div ## "main-table" #. "table-container rt-view-main-table-container" #+
            [ UI.table #. "table rt-view-main-table" #+ []
            ]
        ]
    {-
    , UI.mkElement "section" #. "section" #+
        [ UI.div #. "table-container rt-view-peers-table-container" #+
            [ UI.table #. "table rt-view-peers-table" #+
                [ UI.mkElement "thead" #+
                    [ UI.tr #+
                        [ UI.th #+ [UI.span # set html "&nbsp;"]
                        , UI.th #+ [string "Node 1 ", UI.span #. "tag is-info" # set text "Producer"]
                        , UI.th #+ [string "Node 2 ", UI.span #. "tag is-warning" # set text "Relay"]
                        , UI.th #+ [string "Node 3 ", UI.span #. "tag is-warning" # set text "Relay"]
                        ]
                    ]
                , UI.mkElement "tbody" #+
                    [ UI.tr #+
                        [ UI.td #+ [image "rt-view-overview-icon" versionSVG, string "Version"]
                        , UI.td #+ [string "1.31.0"]
                        , UI.td #+ [string "1.31.0"]
                        , UI.td #+ [string "1.31.0"]
                        ]
                    , UI.tr #+
                        [ UI.td #+ [image "rt-view-overview-icon" protocolSVG,string "Protocol"]
                        , UI.td #+ [string "Shelley"]
                        , UI.td #+ [string "Shelley"]
                        , UI.td #+ [string "Shelley"]
                        ]
                    , UI.tr #+
                        [ UI.td #+ [image "rt-view-overview-icon" commitSVG,string "Commit"]
                        , UI.td #+ [
                                    UI.anchor #. "rt-view-href has-tooltip-multiline has-tooltip-right"
                                              # set UI.href "#"
                                              # set dataTooltip "Browse cardano-node repository on this commit"
                                              # set UI.text "df98476"]
                        , UI.td #+ [
                                    UI.anchor #. "rt-view-href has-tooltip-multiline has-tooltip-right"
                                              # set UI.href "#"
                                              # set dataTooltip "Browse cardano-node repository on this commit"
                                              # set UI.text "df98476"]
                        , UI.td #+ [
                                    UI.anchor #. "rt-view-href has-tooltip-multiline has-tooltip-right"
                                              # set UI.href "#"
                                              # set dataTooltip "Browse cardano-node repository on this commit"
                                              # set UI.text "df98476"]
                        ]
                    , UI.tr #+
                        [ UI.td #+ [image "rt-view-overview-icon" platformSVG, string "Platform"]
                        , UI.td #+ [string "Linux"]
                        , UI.td #+ [string "Linux"]
                        , UI.td #+ [string "Linux"]
                        ]
                    , UI.tr #+
                        [ UI.td #+ [image "rt-view-overview-icon" peersSVG, string "Peers"]
                        , UI.td #+ [UI.span #. "tag is-info is-medium is-light" # set text "2"]
                        , UI.td #+ [UI.span #. "tag is-info is-medium is-light" # set text "2"]
                        , UI.td #+ [UI.span #. "tag is-info is-medium is-light" # set text "2"]
                        ]
                    , UI.tr #+
                        [ UI.td #+ [image "rt-view-overview-icon" chainSVG, string "Chain"]
                        , UI.td #+ [UI.span #. "tag is-info is-medium is-light" # set text "235 / 452333 / 11.1%"]
                        , UI.td #+ [UI.span #. "tag is-info is-medium is-light" # set text "235 / 452333 / 11.8%"]
                        , UI.td #+ [UI.span #. "tag is-info is-medium is-light" # set text "235 / 452333 / 10.2%"]
                        ]
                    , UI.tr #+
                        [ UI.td #+ [image "rt-view-overview-icon" kesSVG, string "Current KES Period"]
                        , UI.td #+ [string "353"]
                        , UI.td #+ [string "353"]
                        , UI.td #+ [string "353"]
                        ]
                    , UI.tr #+
                        [ UI.td #+ [image "rt-view-overview-icon" errorsSVG, string "Errors"]
                        , UI.td #+ [UI.span #. "has-text-success" # set text "✓ No errors"]
                        , UI.td #+ [UI.span #. "tag is-danger is-medium" # set text "3"]
                        , UI.td #+ [UI.span #. "tag is-danger is-medium" # set text "10"]
                        ]
                    , UI.tr #+
                        [ UI.td #+ [image "rt-view-overview-icon" cpuSVG, string "CPU Utilization"]
                        , UI.td #+ [UI.span #. "tag is-info is-medium is-light" # set text "13%"]
                        , UI.td #+ [UI.span #. "tag is-info is-medium is-light" # set text "14%"]
                        , UI.td #+ [UI.span #. "tag is-info is-medium is-light" # set text "25%"]
                        ]
                    ]
                ]
            ]
        ]
        -}
        --UI.div ## "no-nodes" #. "container is-max-widescreen has-text-centered" #+
        --    [ image "rt-view-no-nodes-icon" noNodesSVG
        --    , UI.p #. "rt-view-no-nodes-message" #+
        --        [ string "There are no connected nodes. Yet."
        --        ]
        --    ]
        --, UI.div ## "nodes-panels"
        --         #. "container is-max-widescreen rt-view-nodes-panels" #+ []
        --]
    ]

topNavigation :: UI Element
topNavigation = do
  closeInfo <- UI.button #. "modal-close is-large" #+ []
  info <- mkOwnInfo closeInfo
  infoIcon <- image "mr-4 rt-view-info-icon" rtViewInfoSVG # set UI.title__ "RTView info"
  registerClicksForModal info infoIcon closeInfo

  --closeNotifications <- UI.button #. "modal-close is-large" #+ []
  --notifications <- mkOwnInfo closeNotifications
  notifyIcon <- image "rt-view-notify-icon" rtViewNotifySVG # set UI.title__ "RTView notifications"
  --registerClicksForModal notifications notifyIcon closeNotifications

  UI.div #. "navbar rt-view-top-bar" #+
    [ element info
    -- , element notifications
    , UI.div #. "navbar-brand" #+
        [ UI.div #. "navbar-item" #+
            [ image "rt-view-cardano-logo" cardanoLogoSVG
            , UI.span #. "rt-view-name" # set text "Node Real-time View"
            ]
        ]
    , UI.div #. "navbar-menu" #+
        [ UI.div #. "navbar-start" #+ []
        , UI.div #. "navbar-end" #+
            [ UI.div #. "navbar-item" #+ [ element notifyIcon ]
            , UI.div #. "navbar-item" #+ [ element infoIcon ]
            ]
        ]
    ]
 where
  registerClicksForModal modal iconToOpen iconToClose = do
    on UI.click iconToOpen  $ const $ element modal #. "modal is-active"
    on UI.click iconToClose $ const $ element modal #. "modal"
