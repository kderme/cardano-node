{-# LANGUAGE QuasiQuotes #-}

module Cardano.Tracer.Handlers.RTView.UI.Img.Icons
  ( blockSVG
  , blockchainSVG
  , calendarSVG
  , cardanoLogoSVG
  , chainSVG
  , clockSVG
  , collapsedSVG
  , commitSVG
  , connectedSVG
  , cpuSVG
  , downSVG
  , epochSlotSVG
  , errorsSVG
  , faviconSVGBase64
  , hammerSVG
  , healthSVG
  , kesSVG
  , leaderSVG
  , linuxSVG
  , noNodesSVG
  , overviewSVG
  , peersSVG
  , peersNumSVG
  , platformSVG
  , protocolSVG
  , remainingSVG
  , rtsGCSVG
  , rtViewInfoSVG
  , rtViewNotifySVG
  , sadSVG
  , questionSVG
  , versionSVG
  ) where

import           Data.String.QQ

cardanoLogoSVG :: String
cardanoLogoSVG = [s|
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 375 346.51"><g id="Layer_2" data-name="Layer 2"><g id="Layer_1-2" data-name="Layer 1"><path d="M102.76,172a25.31,25.31,0,0,0,23.78,26.65c.49,0,1,0,1.46,0A25.26,25.26,0,1,0,102.76,172Z" fill="currentColor"/><path d="M8.62,165.5a8.16,8.16,0,1,0,7.69,8.61A8.15,8.15,0,0,0,8.62,165.5Z" fill="currentColor"/><path d="M101.16,25.43a8.16,8.16,0,1,0-11-3.62A8.18,8.18,0,0,0,101.16,25.43Z" fill="currentColor"/><path d="M126.78,70.1a12.61,12.61,0,1,0-16.94-5.59A12.62,12.62,0,0,0,126.78,70.1Z" fill="currentColor"/><path d="M40.58,100.82a10.39,10.39,0,1,0-3-14.38A10.39,10.39,0,0,0,40.58,100.82Z" fill="currentColor"/><path d="M55.93,161a12.62,12.62,0,1,0,11.88,13.31A12.62,12.62,0,0,0,55.93,161Z" fill="currentColor"/><path d="M42,245.72a10.39,10.39,0,1,0,13.95,4.6A10.37,10.37,0,0,0,42,245.72Z" fill="currentColor"/><path d="M91,134.89a14.84,14.84,0,1,0-4.27-20.55A14.83,14.83,0,0,0,91,134.89Z" fill="currentColor"/><path d="M246.47,69.1a12.62,12.62,0,1,0-3.63-17.47A12.61,12.61,0,0,0,246.47,69.1Z" fill="currentColor"/><path d="M272.35,24.57A8.16,8.16,0,1,0,270,13.26,8.16,8.16,0,0,0,272.35,24.57Z" fill="currentColor"/><path d="M248.45,147.91a25.25,25.25,0,0,0-2.87,50.42c.49,0,1,0,1.45,0a25.25,25.25,0,0,0,1.42-50.46Z" fill="currentColor"/><path d="M135.08,133.14A25.12,25.12,0,0,0,157.64,147a25.25,25.25,0,0,0,22.54-36.62,25.25,25.25,0,1,0-45.1,22.73Z" fill="currentColor"/><path d="M333,100.79a10.39,10.39,0,1,0-14-4.6A10.4,10.4,0,0,0,333,100.79Z" fill="currentColor"/><path d="M269,108.83a14.84,14.84,0,1,0,19.94,6.58A14.86,14.86,0,0,0,269,108.83Z" fill="currentColor"/><path d="M186.55,20.76a10.39,10.39,0,1,0-9.79-11A10.38,10.38,0,0,0,186.55,20.76Z" fill="currentColor"/><path d="M186.43,86.13a14.84,14.84,0,1,0-14-15.66A14.84,14.84,0,0,0,186.43,86.13Z" fill="currentColor"/><path d="M106,237.68a14.84,14.84,0,1,0-19.93-6.58A14.85,14.85,0,0,0,106,237.68Z" fill="currentColor"/><path d="M196,107.79a25.22,25.22,0,1,0,21.14-11.41A25.28,25.28,0,0,0,196,107.79Z" fill="currentColor"/><path d="M239.92,213.37a25.26,25.26,0,1,0-11.18,33.91A25.11,25.11,0,0,0,239.92,213.37Z" fill="currentColor"/><path d="M284,211.62a14.84,14.84,0,1,0,4.27,20.55A14.84,14.84,0,0,0,284,211.62Z" fill="currentColor"/><path d="M332.38,173.68a12.62,12.62,0,1,0-13.31,11.88A12.62,12.62,0,0,0,332.38,173.68Z" fill="currentColor"/><path d="M367.3,164.71a8.16,8.16,0,1,0,7.69,8.61A8.17,8.17,0,0,0,367.3,164.71Z" fill="currentColor"/><path d="M334.42,245.68a10.39,10.39,0,1,0,3,14.39A10.39,10.39,0,0,0,334.42,245.68Z" fill="currentColor"/><path d="M102.65,321.94a8.16,8.16,0,1,0,2.34,11.3A8.17,8.17,0,0,0,102.65,321.94Z" fill="currentColor"/><path d="M273.83,321.08a8.16,8.16,0,1,0,11,3.62A8.16,8.16,0,0,0,273.83,321.08Z" fill="currentColor"/><path d="M179,238.71a25.25,25.25,0,1,0-21.14,11.41A25.1,25.1,0,0,0,179,238.71Z" fill="currentColor"/><path d="M128.53,277.41a12.62,12.62,0,1,0,3.63,17.47A12.62,12.62,0,0,0,128.53,277.41Z" fill="currentColor"/><path d="M187.38,325.74a10.39,10.39,0,1,0,9.78,11A10.39,10.39,0,0,0,187.38,325.74Z" fill="currentColor"/><path d="M187.49,260.37a14.84,14.84,0,1,0,14,15.67A14.85,14.85,0,0,0,187.49,260.37Z" fill="currentColor"/><path d="M248.21,276.4a12.62,12.62,0,1,0,17,5.59A12.62,12.62,0,0,0,248.21,276.4Z" fill="currentColor"/></g></g></svg>
|]

rtViewInfoSVG :: String
rtViewInfoSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="info-circle" class="svg-inline--fa fa-info-circle fa-w-16" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path fill="currentColor" d="M256 8C119.043 8 8 119.083 8 256c0 136.997 111.043 248 248 248s248-111.003 248-248C504 119.083 392.957 8 256 8zm0 110c23.196 0 42 18.804 42 42s-18.804 42-42 42-42-18.804-42-42 18.804-42 42-42zm56 254c0 6.627-5.373 12-12 12h-88c-6.627 0-12-5.373-12-12v-24c0-6.627 5.373-12 12-12h12v-64h-12c-6.627 0-12-5.373-12-12v-24c0-6.627 5.373-12 12-12h64c6.627 0 12 5.373 12 12v100h12c6.627 0 12 5.373 12 12v24z"></path></svg>
|]

rtViewNotifySVG :: String
rtViewNotifySVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="bell" class="svg-inline--fa fa-bell fa-w-14" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path fill="currentColor" d="M224 512c35.32 0 63.97-28.65 63.97-64H160.03c0 35.35 28.65 64 63.97 64zm215.39-149.71c-19.32-20.76-55.47-51.99-55.47-154.29 0-77.7-54.48-139.9-127.94-155.16V32c0-17.67-14.32-32-31.98-32s-31.98 14.33-31.98 32v20.84C118.56 68.1 64.08 130.3 64.08 208c0 102.3-36.15 133.53-55.47 154.29-6 6.45-8.66 14.16-8.61 21.71.11 16.4 12.98 32 32.1 32h383.8c19.12 0 32-15.6 32.1-32 .05-7.55-2.61-15.27-8.61-21.71z"></path></svg>
|]

questionSVG :: String
questionSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="far" data-icon="question-circle" class="svg-inline--fa fa-question-circle fa-w-16" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path fill="currentColor" d="M256 8C119.043 8 8 119.083 8 256c0 136.997 111.043 248 248 248s248-111.003 248-248C504 119.083 392.957 8 256 8zm0 448c-110.532 0-200-89.431-200-200 0-110.495 89.472-200 200-200 110.491 0 200 89.471 200 200 0 110.53-89.431 200-200 200zm107.244-255.2c0 67.052-72.421 68.084-72.421 92.863V300c0 6.627-5.373 12-12 12h-45.647c-6.627 0-12-5.373-12-12v-8.659c0-35.745 27.1-50.034 47.579-61.516 17.561-9.845 28.324-16.541 28.324-29.579 0-17.246-21.999-28.693-39.784-28.693-23.189 0-33.894 10.977-48.942 29.969-4.057 5.12-11.46 6.071-16.666 2.124l-27.824-21.098c-5.107-3.872-6.251-11.066-2.644-16.363C184.846 131.491 214.94 112 261.794 112c49.071 0 101.45 38.304 101.45 88.8zM298 368c0 23.159-18.841 42-42 42s-42-18.841-42-42 18.841-42 42-42 42 18.841 42 42z"></path></svg>
|]

downSVG :: String
downSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="angle-down" class="svg-inline--fa fa-angle-down fa-w-10" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 320 512"><path fill="currentColor" d="M143 352.3L7 216.3c-9.4-9.4-9.4-24.6 0-33.9l22.6-22.6c9.4-9.4 24.6-9.4 33.9 0l96.4 96.4 96.4-96.4c9.4-9.4 24.6-9.4 33.9 0l22.6 22.6c9.4 9.4 9.4 24.6 0 33.9l-136 136c-9.2 9.4-24.4 9.4-33.8 0z"></path></svg>
|]

collapsedSVG :: String
collapsedSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="angle-left" class="svg-inline--fa fa-angle-left fa-w-8" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 256 512"><path fill="currentColor" d="M31.7 239l136-136c9.4-9.4 24.6-9.4 33.9 0l22.6 22.6c9.4 9.4 9.4 24.6 0 33.9L127.9 256l96.4 96.4c9.4 9.4 9.4 24.6 0 33.9L201.7 409c-9.4 9.4-24.6 9.4-33.9 0l-136-136c-9.5-9.4-9.5-24.6-.1-34z"></path></svg>
|]

protocolSVG :: String
protocolSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="far" data-icon="handshake" class="svg-inline--fa fa-handshake fa-w-20" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 640 512"><path fill="currentColor" d="M519.2 127.9l-47.6-47.6A56.252 56.252 0 0 0 432 64H205.2c-14.8 0-29.1 5.9-39.6 16.3L118 127.9H0v255.7h64c17.6 0 31.8-14.2 31.9-31.7h9.1l84.6 76.4c30.9 25.1 73.8 25.7 105.6 3.8 12.5 10.8 26 15.9 41.1 15.9 18.2 0 35.3-7.4 48.8-24 22.1 8.7 48.2 2.6 64-16.8l26.2-32.3c5.6-6.9 9.1-14.8 10.9-23h57.9c.1 17.5 14.4 31.7 31.9 31.7h64V127.9H519.2zM48 351.6c-8.8 0-16-7.2-16-16s7.2-16 16-16 16 7.2 16 16c0 8.9-7.2 16-16 16zm390-6.9l-26.1 32.2c-2.8 3.4-7.8 4-11.3 1.2l-23.9-19.4-30 36.5c-6 7.3-15 4.8-18 2.4l-36.8-31.5-15.6 19.2c-13.9 17.1-39.2 19.7-55.3 6.6l-97.3-88H96V175.8h41.9l61.7-61.6c2-.8 3.7-1.5 5.7-2.3H262l-38.7 35.5c-29.4 26.9-31.1 72.3-4.4 101.3 14.8 16.2 61.2 41.2 101.5 4.4l8.2-7.5 108.2 87.8c3.4 2.8 3.9 7.9 1.2 11.3zm106-40.8h-69.2c-2.3-2.8-4.9-5.4-7.7-7.7l-102.7-83.4 12.5-11.4c6.5-6 7-16.1 1-22.6L367 167.1c-6-6.5-16.1-6.9-22.6-1l-55.2 50.6c-9.5 8.7-25.7 9.4-34.6 0-9.3-9.9-8.5-25.1 1.2-33.9l65.6-60.1c7.4-6.8 17-10.5 27-10.5l83.7-.2c2.1 0 4.1.8 5.5 2.3l61.7 61.6H544v128zm48 47.7c-8.8 0-16-7.2-16-16s7.2-16 16-16 16 7.2 16 16c0 8.9-7.2 16-16 16z"></path></svg>
|]

versionSVG :: String
versionSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="tag" class="svg-inline--fa fa-tag fa-w-16" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path fill="currentColor" d="M0 252.118V48C0 21.49 21.49 0 48 0h204.118a48 48 0 0 1 33.941 14.059l211.882 211.882c18.745 18.745 18.745 49.137 0 67.882L293.823 497.941c-18.745 18.745-49.137 18.745-67.882 0L14.059 286.059A48 48 0 0 1 0 252.118zM112 64c-26.51 0-48 21.49-48 48s21.49 48 48 48 48-21.49 48-48-21.49-48-48-48z"></path></svg>
|]

commitSVG :: String
commitSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="code-branch" class="svg-inline--fa fa-code-branch fa-w-12" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 384 512"><path fill="currentColor" d="M384 144c0-44.2-35.8-80-80-80s-80 35.8-80 80c0 36.4 24.3 67.1 57.5 76.8-.6 16.1-4.2 28.5-11 36.9-15.4 19.2-49.3 22.4-85.2 25.7-28.2 2.6-57.4 5.4-81.3 16.9v-144c32.5-10.2 56-40.5 56-76.3 0-44.2-35.8-80-80-80S0 35.8 0 80c0 35.8 23.5 66.1 56 76.3v199.3C23.5 365.9 0 396.2 0 432c0 44.2 35.8 80 80 80s80-35.8 80-80c0-34-21.2-63.1-51.2-74.6 3.1-5.2 7.8-9.8 14.9-13.4 16.2-8.2 40.4-10.4 66.1-12.8 42.2-3.9 90-8.4 118.2-43.4 14-17.4 21.1-39.8 21.6-67.9 31.6-10.8 54.4-40.7 54.4-75.9zM80 64c8.8 0 16 7.2 16 16s-7.2 16-16 16-16-7.2-16-16 7.2-16 16-16zm0 384c-8.8 0-16-7.2-16-16s7.2-16 16-16 16 7.2 16 16-7.2 16-16 16zm224-320c8.8 0 16 7.2 16 16s-7.2 16-16 16-16-7.2-16-16 7.2-16 16-16z"></path></svg>
|]

linuxSVG :: String
linuxSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fab" data-icon="linux" class="svg-inline--fa fa-linux fa-w-14" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path fill="currentColor" d="M220.8 123.3c1 .5 1.8 1.7 3 1.7 1.1 0 2.8-.4 2.9-1.5.2-1.4-1.9-2.3-3.2-2.9-1.7-.7-3.9-1-5.5-.1-.4.2-.8.7-.6 1.1.3 1.3 2.3 1.1 3.4 1.7zm-21.9 1.7c1.2 0 2-1.2 3-1.7 1.1-.6 3.1-.4 3.5-1.6.2-.4-.2-.9-.6-1.1-1.6-.9-3.8-.6-5.5.1-1.3.6-3.4 1.5-3.2 2.9.1 1 1.8 1.5 2.8 1.4zM420 403.8c-3.6-4-5.3-11.6-7.2-19.7-1.8-8.1-3.9-16.8-10.5-22.4-1.3-1.1-2.6-2.1-4-2.9-1.3-.8-2.7-1.5-4.1-2 9.2-27.3 5.6-54.5-3.7-79.1-11.4-30.1-31.3-56.4-46.5-74.4-17.1-21.5-33.7-41.9-33.4-72C311.1 85.4 315.7.1 234.8 0 132.4-.2 158 103.4 156.9 135.2c-1.7 23.4-6.4 41.8-22.5 64.7-18.9 22.5-45.5 58.8-58.1 96.7-6 17.9-8.8 36.1-6.2 53.3-6.5 5.8-11.4 14.7-16.6 20.2-4.2 4.3-10.3 5.9-17 8.3s-14 6-18.5 14.5c-2.1 3.9-2.8 8.1-2.8 12.4 0 3.9.6 7.9 1.2 11.8 1.2 8.1 2.5 15.7.8 20.8-5.2 14.4-5.9 24.4-2.2 31.7 3.8 7.3 11.4 10.5 20.1 12.3 17.3 3.6 40.8 2.7 59.3 12.5 19.8 10.4 39.9 14.1 55.9 10.4 11.6-2.6 21.1-9.6 25.9-20.2 12.5-.1 26.3-5.4 48.3-6.6 14.9-1.2 33.6 5.3 55.1 4.1.6 2.3 1.4 4.6 2.5 6.7v.1c8.3 16.7 23.8 24.3 40.3 23 16.6-1.3 34.1-11 48.3-27.9 13.6-16.4 36-23.2 50.9-32.2 7.4-4.5 13.4-10.1 13.9-18.3.4-8.2-4.4-17.3-15.5-29.7zM223.7 87.3c9.8-22.2 34.2-21.8 44-.4 6.5 14.2 3.6 30.9-4.3 40.4-1.6-.8-5.9-2.6-12.6-4.9 1.1-1.2 3.1-2.7 3.9-4.6 4.8-11.8-.2-27-9.1-27.3-7.3-.5-13.9 10.8-11.8 23-4.1-2-9.4-3.5-13-4.4-1-6.9-.3-14.6 2.9-21.8zM183 75.8c10.1 0 20.8 14.2 19.1 33.5-3.5 1-7.1 2.5-10.2 4.6 1.2-8.9-3.3-20.1-9.6-19.6-8.4.7-9.8 21.2-1.8 28.1 1 .8 1.9-.2-5.9 5.5-15.6-14.6-10.5-52.1 8.4-52.1zm-13.6 60.7c6.2-4.6 13.6-10 14.1-10.5 4.7-4.4 13.5-14.2 27.9-14.2 7.1 0 15.6 2.3 25.9 8.9 6.3 4.1 11.3 4.4 22.6 9.3 8.4 3.5 13.7 9.7 10.5 18.2-2.6 7.1-11 14.4-22.7 18.1-11.1 3.6-19.8 16-38.2 14.9-3.9-.2-7-1-9.6-2.1-8-3.5-12.2-10.4-20-15-8.6-4.8-13.2-10.4-14.7-15.3-1.4-4.9 0-9 4.2-12.3zm3.3 334c-2.7 35.1-43.9 34.4-75.3 18-29.9-15.8-68.6-6.5-76.5-21.9-2.4-4.7-2.4-12.7 2.6-26.4v-.2c2.4-7.6.6-16-.6-23.9-1.2-7.8-1.8-15 .9-20 3.5-6.7 8.5-9.1 14.8-11.3 10.3-3.7 11.8-3.4 19.6-9.9 5.5-5.7 9.5-12.9 14.3-18 5.1-5.5 10-8.1 17.7-6.9 8.1 1.2 15.1 6.8 21.9 16l19.6 35.6c9.5 19.9 43.1 48.4 41 68.9zm-1.4-25.9c-4.1-6.6-9.6-13.6-14.4-19.6 7.1 0 14.2-2.2 16.7-8.9 2.3-6.2 0-14.9-7.4-24.9-13.5-18.2-38.3-32.5-38.3-32.5-13.5-8.4-21.1-18.7-24.6-29.9s-3-23.3-.3-35.2c5.2-22.9 18.6-45.2 27.2-59.2 2.3-1.7.8 3.2-8.7 20.8-8.5 16.1-24.4 53.3-2.6 82.4.6-20.7 5.5-41.8 13.8-61.5 12-27.4 37.3-74.9 39.3-112.7 1.1.8 4.6 3.2 6.2 4.1 4.6 2.7 8.1 6.7 12.6 10.3 12.4 10 28.5 9.2 42.4 1.2 6.2-3.5 11.2-7.5 15.9-9 9.9-3.1 17.8-8.6 22.3-15 7.7 30.4 25.7 74.3 37.2 95.7 6.1 11.4 18.3 35.5 23.6 64.6 3.3-.1 7 .4 10.9 1.4 13.8-35.7-11.7-74.2-23.3-84.9-4.7-4.6-4.9-6.6-2.6-6.5 12.6 11.2 29.2 33.7 35.2 59 2.8 11.6 3.3 23.7.4 35.7 16.4 6.8 35.9 17.9 30.7 34.8-2.2-.1-3.2 0-4.2 0 3.2-10.1-3.9-17.6-22.8-26.1-19.6-8.6-36-8.6-38.3 12.5-12.1 4.2-18.3 14.7-21.4 27.3-2.8 11.2-3.6 24.7-4.4 39.9-.5 7.7-3.6 18-6.8 29-32.1 22.9-76.7 32.9-114.3 7.2zm257.4-11.5c-.9 16.8-41.2 19.9-63.2 46.5-13.2 15.7-29.4 24.4-43.6 25.5s-26.5-4.8-33.7-19.3c-4.7-11.1-2.4-23.1 1.1-36.3 3.7-14.2 9.2-28.8 9.9-40.6.8-15.2 1.7-28.5 4.2-38.7 2.6-10.3 6.6-17.2 13.7-21.1.3-.2.7-.3 1-.5.8 13.2 7.3 26.6 18.8 29.5 12.6 3.3 30.7-7.5 38.4-16.3 9-.3 15.7-.9 22.6 5.1 9.9 8.5 7.1 30.3 17.1 41.6 10.6 11.6 14 19.5 13.7 24.6zM173.3 148.7c2 1.9 4.7 4.5 8 7.1 6.6 5.2 15.8 10.6 27.3 10.6 11.6 0 22.5-5.9 31.8-10.8 4.9-2.6 10.9-7 14.8-10.4s5.9-6.3 3.1-6.6-2.6 2.6-6 5.1c-4.4 3.2-9.7 7.4-13.9 9.8-7.4 4.2-19.5 10.2-29.9 10.2s-18.7-4.8-24.9-9.7c-3.1-2.5-5.7-5-7.7-6.9-1.5-1.4-1.9-4.6-4.3-4.9-1.4-.1-1.8 3.7 1.7 6.5z"></path></svg>
|]

calendarSVG :: String
calendarSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="far" data-icon="calendar-alt" class="svg-inline--fa fa-calendar-alt fa-w-14" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path fill="currentColor" d="M148 288h-40c-6.6 0-12-5.4-12-12v-40c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v40c0 6.6-5.4 12-12 12zm108-12v-40c0-6.6-5.4-12-12-12h-40c-6.6 0-12 5.4-12 12v40c0 6.6 5.4 12 12 12h40c6.6 0 12-5.4 12-12zm96 0v-40c0-6.6-5.4-12-12-12h-40c-6.6 0-12 5.4-12 12v40c0 6.6 5.4 12 12 12h40c6.6 0 12-5.4 12-12zm-96 96v-40c0-6.6-5.4-12-12-12h-40c-6.6 0-12 5.4-12 12v40c0 6.6 5.4 12 12 12h40c6.6 0 12-5.4 12-12zm-96 0v-40c0-6.6-5.4-12-12-12h-40c-6.6 0-12 5.4-12 12v40c0 6.6 5.4 12 12 12h40c6.6 0 12-5.4 12-12zm192 0v-40c0-6.6-5.4-12-12-12h-40c-6.6 0-12 5.4-12 12v40c0 6.6 5.4 12 12 12h40c6.6 0 12-5.4 12-12zm96-260v352c0 26.5-21.5 48-48 48H48c-26.5 0-48-21.5-48-48V112c0-26.5 21.5-48 48-48h48V12c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v52h128V12c0-6.6 5.4-12 12-12h40c6.6 0 12 5.4 12 12v52h48c26.5 0 48 21.5 48 48zm-48 346V160H48v298c0 3.3 2.7 6 6 6h340c3.3 0 6-2.7 6-6z"></path></svg>
|]

clockSVG :: String
clockSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="far" data-icon="clock" class="svg-inline--fa fa-clock fa-w-16" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path fill="currentColor" d="M256 8C119 8 8 119 8 256s111 248 248 248 248-111 248-248S393 8 256 8zm0 448c-110.5 0-200-89.5-200-200S145.5 56 256 56s200 89.5 200 200-89.5 200-200 200zm61.8-104.4l-84.9-61.7c-3.1-2.3-4.9-5.9-4.9-9.7V116c0-6.6 5.4-12 12-12h32c6.6 0 12 5.4 12 12v141.7l66.8 48.6c5.4 3.9 6.5 11.4 2.6 16.8L334.6 349c-3.9 5.3-11.4 6.5-16.8 2.6z"></path></svg>
|]

healthSVG :: String
healthSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="stethoscope" class="svg-inline--fa fa-stethoscope fa-w-16" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path fill="currentColor" d="M447.1 112c-34.2.5-62.3 28.4-63 62.6-.5 24.3 12.5 45.6 32 56.8V344c0 57.3-50.2 104-112 104-60 0-109.2-44.1-111.9-99.2C265 333.8 320 269.2 320 192V36.6c0-11.4-8.1-21.3-19.3-23.5L237.8.5c-13-2.6-25.6 5.8-28.2 18.8L206.4 35c-2.6 13 5.8 25.6 18.8 28.2l30.7 6.1v121.4c0 52.9-42.2 96.7-95.1 97.2-53.4.5-96.9-42.7-96.9-96V69.4l30.7-6.1c13-2.6 21.4-15.2 18.8-28.2l-3.1-15.7C107.7 6.4 95.1-2 82.1.6L19.3 13C8.1 15.3 0 25.1 0 36.6V192c0 77.3 55.1 142 128.1 156.8C130.7 439.2 208.6 512 304 512c97 0 176-75.4 176-168V231.4c19.1-11.1 32-31.7 32-55.4 0-35.7-29.2-64.5-64.9-64zm.9 80c-8.8 0-16-7.2-16-16s7.2-16 16-16 16 7.2 16 16-7.2 16-16 16z"></path></svg>
|]

overviewSVG :: String
overviewSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="info" class="svg-inline--fa fa-info fa-w-6" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 192 512"><path fill="currentColor" d="M20 424.229h20V279.771H20c-11.046 0-20-8.954-20-20V212c0-11.046 8.954-20 20-20h112c11.046 0 20 8.954 20 20v212.229h20c11.046 0 20 8.954 20 20V492c0 11.046-8.954 20-20 20H20c-11.046 0-20-8.954-20-20v-47.771c0-11.046 8.954-20 20-20zM96 0C56.235 0 24 32.235 24 72s32.235 72 72 72 72-32.235 72-72S135.764 0 96 0z"></path></svg>
|]

kesSVG :: String
kesSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="key" class="svg-inline--fa fa-key fa-w-16" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path fill="currentColor" d="M512 176.001C512 273.203 433.202 352 336 352c-11.22 0-22.19-1.062-32.827-3.069l-24.012 27.014A23.999 23.999 0 0 1 261.223 384H224v40c0 13.255-10.745 24-24 24h-40v40c0 13.255-10.745 24-24 24H24c-13.255 0-24-10.745-24-24v-78.059c0-6.365 2.529-12.47 7.029-16.971l161.802-161.802C163.108 213.814 160 195.271 160 176 160 78.798 238.797.001 335.999 0 433.488-.001 512 78.511 512 176.001zM336 128c0 26.51 21.49 48 48 48s48-21.49 48-48-21.49-48-48-48-48 21.49-48 48z"></path></svg>
|]

peersSVG :: String
peersSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="network-wired" class="svg-inline--fa fa-network-wired fa-w-20" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 640 512"><path fill="currentColor" d="M640 264v-16c0-8.84-7.16-16-16-16H344v-40h72c17.67 0 32-14.33 32-32V32c0-17.67-14.33-32-32-32H224c-17.67 0-32 14.33-32 32v128c0 17.67 14.33 32 32 32h72v40H16c-8.84 0-16 7.16-16 16v16c0 8.84 7.16 16 16 16h104v40H64c-17.67 0-32 14.33-32 32v128c0 17.67 14.33 32 32 32h160c17.67 0 32-14.33 32-32V352c0-17.67-14.33-32-32-32h-56v-40h304v40h-56c-17.67 0-32 14.33-32 32v128c0 17.67 14.33 32 32 32h160c17.67 0 32-14.33 32-32V352c0-17.67-14.33-32-32-32h-56v-40h104c8.84 0 16-7.16 16-16zM256 128V64h128v64H256zm-64 320H96v-64h96v64zm352 0h-96v-64h96v64z"></path></svg>
|]

blockchainSVG :: String
blockchainSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="cubes" class="svg-inline--fa fa-cubes fa-w-16" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path fill="currentColor" d="M488.6 250.2L392 214V105.5c0-15-9.3-28.4-23.4-33.7l-100-37.5c-8.1-3.1-17.1-3.1-25.3 0l-100 37.5c-14.1 5.3-23.4 18.7-23.4 33.7V214l-96.6 36.2C9.3 255.5 0 268.9 0 283.9V394c0 13.6 7.7 26.1 19.9 32.2l100 50c10.1 5.1 22.1 5.1 32.2 0l103.9-52 103.9 52c10.1 5.1 22.1 5.1 32.2 0l100-50c12.2-6.1 19.9-18.6 19.9-32.2V283.9c0-15-9.3-28.4-23.4-33.7zM358 214.8l-85 31.9v-68.2l85-37v73.3zM154 104.1l102-38.2 102 38.2v.6l-102 41.4-102-41.4v-.6zm84 291.1l-85 42.5v-79.1l85-38.8v75.4zm0-112l-102 41.4-102-41.4v-.6l102-38.2 102 38.2v.6zm240 112l-85 42.5v-79.1l85-38.8v75.4zm0-112l-102 41.4-102-41.4v-.6l102-38.2 102 38.2v.6z"></path></svg>
|]

errorsSVG :: String
errorsSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="bug" class="svg-inline--fa fa-bug fa-w-16" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path fill="currentColor" d="M511.988 288.9c-.478 17.43-15.217 31.1-32.653 31.1H424v16c0 21.864-4.882 42.584-13.6 61.145l60.228 60.228c12.496 12.497 12.496 32.758 0 45.255-12.498 12.497-32.759 12.496-45.256 0l-54.736-54.736C345.886 467.965 314.351 480 280 480V236c0-6.627-5.373-12-12-12h-24c-6.627 0-12 5.373-12 12v244c-34.351 0-65.886-12.035-90.636-32.108l-54.736 54.736c-12.498 12.497-32.759 12.496-45.256 0-12.496-12.497-12.496-32.758 0-45.255l60.228-60.228C92.882 378.584 88 357.864 88 336v-16H32.666C15.23 320 .491 306.33.013 288.9-.484 270.816 14.028 256 32 256h56v-58.745l-46.628-46.628c-12.496-12.497-12.496-32.758 0-45.255 12.498-12.497 32.758-12.497 45.256 0L141.255 160h229.489l54.627-54.627c12.498-12.497 32.758-12.497 45.256 0 12.496 12.497 12.496 32.758 0 45.255L424 197.255V256h56c17.972 0 32.484 14.816 31.988 32.9zM257 0c-61.856 0-112 50.144-112 112h224C369 50.144 318.856 0 257 0z"></path></svg>
|]

rtsGCSVG :: String
rtsGCSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="recycle" class="svg-inline--fa fa-recycle fa-w-16" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path fill="currentColor" d="M184.561 261.903c3.232 13.997-12.123 24.635-24.068 17.168l-40.736-25.455-50.867 81.402C55.606 356.273 70.96 384 96.012 384H148c6.627 0 12 5.373 12 12v40c0 6.627-5.373 12-12 12H96.115c-75.334 0-121.302-83.048-81.408-146.88l50.822-81.388-40.725-25.448c-12.081-7.547-8.966-25.961 4.879-29.158l110.237-25.45c8.611-1.988 17.201 3.381 19.189 11.99l25.452 110.237zm98.561-182.915l41.289 66.076-40.74 25.457c-12.051 7.528-9 25.953 4.879 29.158l110.237 25.45c8.672 1.999 17.215-3.438 19.189-11.99l25.45-110.237c3.197-13.844-11.99-24.719-24.068-17.168l-40.687 25.424-41.263-66.082c-37.521-60.033-125.209-60.171-162.816 0l-17.963 28.766c-3.51 5.62-1.8 13.021 3.82 16.533l33.919 21.195c5.62 3.512 13.024 1.803 16.536-3.817l17.961-28.743c12.712-20.341 41.973-19.676 54.257-.022zM497.288 301.12l-27.515-44.065c-3.511-5.623-10.916-7.334-16.538-3.821l-33.861 21.159c-5.62 3.512-7.33 10.915-3.818 16.536l27.564 44.112c13.257 21.211-2.057 48.96-27.136 48.96H320V336.02c0-14.213-17.242-21.383-27.313-11.313l-80 79.981c-6.249 6.248-6.249 16.379 0 22.627l80 79.989C302.689 517.308 320 510.3 320 495.989V448h95.88c75.274 0 121.335-82.997 81.408-146.88z"></path></svg>
|]

noNodesSVG :: String
noNodesSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="unlink" class="svg-inline--fa fa-unlink fa-w-16" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path fill="currentColor" d="M304.083 405.907c4.686 4.686 4.686 12.284 0 16.971l-44.674 44.674c-59.263 59.262-155.693 59.266-214.961 0-59.264-59.265-59.264-155.696 0-214.96l44.675-44.675c4.686-4.686 12.284-4.686 16.971 0l39.598 39.598c4.686 4.686 4.686 12.284 0 16.971l-44.675 44.674c-28.072 28.073-28.072 73.75 0 101.823 28.072 28.072 73.75 28.073 101.824 0l44.674-44.674c4.686-4.686 12.284-4.686 16.971 0l39.597 39.598zm-56.568-260.216c4.686 4.686 12.284 4.686 16.971 0l44.674-44.674c28.072-28.075 73.75-28.073 101.824 0 28.072 28.073 28.072 73.75 0 101.823l-44.675 44.674c-4.686 4.686-4.686 12.284 0 16.971l39.598 39.598c4.686 4.686 12.284 4.686 16.971 0l44.675-44.675c59.265-59.265 59.265-155.695 0-214.96-59.266-59.264-155.695-59.264-214.961 0l-44.674 44.674c-4.686 4.686-4.686 12.284 0 16.971l39.597 39.598zm234.828 359.28l22.627-22.627c9.373-9.373 9.373-24.569 0-33.941L63.598 7.029c-9.373-9.373-24.569-9.373-33.941 0L7.029 29.657c-9.373 9.373-9.373 24.569 0 33.941l441.373 441.373c9.373 9.372 24.569 9.372 33.941 0z"></path></svg>
|]

connectedSVG :: String
connectedSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="ethernet" class="svg-inline--fa fa-ethernet fa-w-16" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path fill="currentColor" d="M496 192h-48v-48c0-8.8-7.2-16-16-16h-48V80c0-8.8-7.2-16-16-16H144c-8.8 0-16 7.2-16 16v48H80c-8.8 0-16 7.2-16 16v48H16c-8.8 0-16 7.2-16 16v224c0 8.8 7.2 16 16 16h80V320h32v128h64V320h32v128h64V320h32v128h64V320h32v128h80c8.8 0 16-7.2 16-16V208c0-8.8-7.2-16-16-16z"></path></svg>
|]

remainingSVG :: String
remainingSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="hourglass-half" class="svg-inline--fa fa-hourglass-half fa-w-12" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 384 512"><path fill="currentColor" d="M360 0H24C10.745 0 0 10.745 0 24v16c0 13.255 10.745 24 24 24 0 90.965 51.016 167.734 120.842 192C75.016 280.266 24 357.035 24 448c-13.255 0-24 10.745-24 24v16c0 13.255 10.745 24 24 24h336c13.255 0 24-10.745 24-24v-16c0-13.255-10.745-24-24-24 0-90.965-51.016-167.734-120.842-192C308.984 231.734 360 154.965 360 64c13.255 0 24-10.745 24-24V24c0-13.255-10.745-24-24-24zm-75.078 384H99.08c17.059-46.797 52.096-80 92.92-80 40.821 0 75.862 33.196 92.922 80zm.019-256H99.078C91.988 108.548 88 86.748 88 64h208c0 22.805-3.987 44.587-11.059 64z"></path></svg>
|]

peersNumSVG :: String
peersNumSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="project-diagram" class="svg-inline--fa fa-project-diagram fa-w-20" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 640 512"><path fill="currentColor" d="M384 320H256c-17.67 0-32 14.33-32 32v128c0 17.67 14.33 32 32 32h128c17.67 0 32-14.33 32-32V352c0-17.67-14.33-32-32-32zM192 32c0-17.67-14.33-32-32-32H32C14.33 0 0 14.33 0 32v128c0 17.67 14.33 32 32 32h95.72l73.16 128.04C211.98 300.98 232.4 288 256 288h.28L192 175.51V128h224V64H192V32zM608 0H480c-17.67 0-32 14.33-32 32v128c0 17.67 14.33 32 32 32h128c17.67 0 32-14.33 32-32V32c0-17.67-14.33-32-32-32z"></path></svg>
|]

epochSlotSVG :: String
epochSlotSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="ruler-horizontal" class="svg-inline--fa fa-ruler-horizontal fa-w-18" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 576 512"><path fill="currentColor" d="M544 128h-48v88c0 4.42-3.58 8-8 8h-16c-4.42 0-8-3.58-8-8v-88h-64v88c0 4.42-3.58 8-8 8h-16c-4.42 0-8-3.58-8-8v-88h-64v88c0 4.42-3.58 8-8 8h-16c-4.42 0-8-3.58-8-8v-88h-64v88c0 4.42-3.58 8-8 8h-16c-4.42 0-8-3.58-8-8v-88h-64v88c0 4.42-3.58 8-8 8H88c-4.42 0-8-3.58-8-8v-88H32c-17.67 0-32 14.33-32 32v192c0 17.67 14.33 32 32 32h512c17.67 0 32-14.33 32-32V160c0-17.67-14.33-32-32-32z"></path></svg>
|]

hammerSVG :: String
hammerSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="hammer" class="svg-inline--fa fa-hammer fa-w-18" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 576 512"><path fill="currentColor" d="M571.31 193.94l-22.63-22.63c-6.25-6.25-16.38-6.25-22.63 0l-11.31 11.31-28.9-28.9c5.63-21.31.36-44.9-16.35-61.61l-45.25-45.25c-62.48-62.48-163.79-62.48-226.28 0l90.51 45.25v18.75c0 16.97 6.74 33.25 18.75 45.25l49.14 49.14c16.71 16.71 40.3 21.98 61.61 16.35l28.9 28.9-11.31 11.31c-6.25 6.25-6.25 16.38 0 22.63l22.63 22.63c6.25 6.25 16.38 6.25 22.63 0l90.51-90.51c6.23-6.24 6.23-16.37-.02-22.62zm-286.72-15.2c-3.7-3.7-6.84-7.79-9.85-11.95L19.64 404.96c-25.57 23.88-26.26 64.19-1.53 88.93s65.05 24.05 88.93-1.53l238.13-255.07c-3.96-2.91-7.9-5.87-11.44-9.41l-49.14-49.14z"></path></svg>
|]

leaderSVG :: String
leaderSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="crown" class="svg-inline--fa fa-crown fa-w-20" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 640 512"><path fill="currentColor" d="M528 448H112c-8.8 0-16 7.2-16 16v32c0 8.8 7.2 16 16 16h416c8.8 0 16-7.2 16-16v-32c0-8.8-7.2-16-16-16zm64-320c-26.5 0-48 21.5-48 48 0 7.1 1.6 13.7 4.4 19.8L476 239.2c-15.4 9.2-35.3 4-44.2-11.6L350.3 85C361 76.2 368 63 368 48c0-26.5-21.5-48-48-48s-48 21.5-48 48c0 15 7 28.2 17.7 37l-81.5 142.6c-8.9 15.6-28.9 20.8-44.2 11.6l-72.3-43.4c2.7-6 4.4-12.7 4.4-19.8 0-26.5-21.5-48-48-48S0 149.5 0 176s21.5 48 48 48c2.6 0 5.2-.4 7.7-.8L128 416h384l72.3-192.8c2.5.4 5.1.8 7.7.8 26.5 0 48-21.5 48-48s-21.5-48-48-48z"></path></svg>
|]

chainSVG :: String
chainSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="link" class="svg-inline--fa fa-link fa-w-16" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path fill="currentColor" d="M326.612 185.391c59.747 59.809 58.927 155.698.36 214.59-.11.12-.24.25-.36.37l-67.2 67.2c-59.27 59.27-155.699 59.262-214.96 0-59.27-59.26-59.27-155.7 0-214.96l37.106-37.106c9.84-9.84 26.786-3.3 27.294 10.606.648 17.722 3.826 35.527 9.69 52.721 1.986 5.822.567 12.262-3.783 16.612l-13.087 13.087c-28.026 28.026-28.905 73.66-1.155 101.96 28.024 28.579 74.086 28.749 102.325.51l67.2-67.19c28.191-28.191 28.073-73.757 0-101.83-3.701-3.694-7.429-6.564-10.341-8.569a16.037 16.037 0 0 1-6.947-12.606c-.396-10.567 3.348-21.456 11.698-29.806l21.054-21.055c5.521-5.521 14.182-6.199 20.584-1.731a152.482 152.482 0 0 1 20.522 17.197zM467.547 44.449c-59.261-59.262-155.69-59.27-214.96 0l-67.2 67.2c-.12.12-.25.25-.36.37-58.566 58.892-59.387 154.781.36 214.59a152.454 152.454 0 0 0 20.521 17.196c6.402 4.468 15.064 3.789 20.584-1.731l21.054-21.055c8.35-8.35 12.094-19.239 11.698-29.806a16.037 16.037 0 0 0-6.947-12.606c-2.912-2.005-6.64-4.875-10.341-8.569-28.073-28.073-28.191-73.639 0-101.83l67.2-67.19c28.239-28.239 74.3-28.069 102.325.51 27.75 28.3 26.872 73.934-1.155 101.96l-13.087 13.087c-4.35 4.35-5.769 10.79-3.783 16.612 5.864 17.194 9.042 34.999 9.69 52.721.509 13.906 17.454 20.446 27.294 10.606l37.106-37.106c59.271-59.259 59.271-155.699.001-214.959z"></path></svg>
|]

blockSVG :: String
blockSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="cube" class="svg-inline--fa fa-cube fa-w-16" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path fill="currentColor" d="M239.1 6.3l-208 78c-18.7 7-31.1 25-31.1 45v225.1c0 18.2 10.3 34.8 26.5 42.9l208 104c13.5 6.8 29.4 6.8 42.9 0l208-104c16.3-8.1 26.5-24.8 26.5-42.9V129.3c0-20-12.4-37.9-31.1-44.9l-208-78C262 2.2 250 2.2 239.1 6.3zM256 68.4l192 72v1.1l-192 78-192-78v-1.1l192-72zm32 356V275.5l160-65v133.9l-160 80z"></path></svg>
|]

sadSVG :: String
sadSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="far" data-icon="frown" class="svg-inline--fa fa-frown fa-w-16" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 496 512"><path fill="currentColor" d="M248 8C111 8 0 119 0 256s111 248 248 248 248-111 248-248S385 8 248 8zm0 448c-110.3 0-200-89.7-200-200S137.7 56 248 56s200 89.7 200 200-89.7 200-200 200zm-80-216c17.7 0 32-14.3 32-32s-14.3-32-32-32-32 14.3-32 32 14.3 32 32 32zm160-64c-17.7 0-32 14.3-32 32s14.3 32 32 32 32-14.3 32-32-14.3-32-32-32zm-80 128c-40.2 0-78 17.7-103.8 48.6-8.5 10.2-7.1 25.3 3.1 33.8 10.2 8.4 25.3 7.1 33.8-3.1 16.6-19.9 41-31.4 66.9-31.4s50.3 11.4 66.9 31.4c8.1 9.7 23.1 11.9 33.8 3.1 10.2-8.5 11.5-23.6 3.1-33.8C326 321.7 288.2 304 248 304z"></path></svg>
|]

faviconSVGBase64 :: String
faviconSVGBase64 = [s|
PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAzNzUgMzQ2LjUxIj48ZyBpZD0iTGF5ZXJfMiIgZGF0YS1uYW1lPSJMYXllciAyIj48ZyBpZD0iTGF5ZXJfMS0yIiBkYXRhLW5hbWU9IkxheWVyIDEiPjxwYXRoIGQ9Ik0xMDIuNzYsMTcyYTI1LjMxLDI1LjMxLDAsMCwwLDIzLjc4LDI2LjY1Yy40OSwwLDEsMCwxLjQ2LDBBMjUuMjYsMjUuMjYsMCwxLDAsMTAyLjc2LDE3MloiIGZpbGw9IiMwMDMzYWQiLz48cGF0aCBkPSJNOC42MiwxNjUuNWE4LjE2LDguMTYsMCwxLDAsNy42OSw4LjYxQTguMTUsOC4xNSwwLDAsMCw4LjYyLDE2NS41WiIgZmlsbD0iIzAwMzNhZCIvPjxwYXRoIGQ9Ik0xMDEuMTYsMjUuNDNhOC4xNiw4LjE2LDAsMSwwLTExLTMuNjJBOC4xOCw4LjE4LDAsMCwwLDEwMS4xNiwyNS40M1oiIGZpbGw9IiMwMDMzYWQiLz48cGF0aCBkPSJNMTI2Ljc4LDcwLjFhMTIuNjEsMTIuNjEsMCwxLDAtMTYuOTQtNS41OUExMi42MiwxMi42MiwwLDAsMCwxMjYuNzgsNzAuMVoiIGZpbGw9IiMwMDMzYWQiLz48cGF0aCBkPSJNNDAuNTgsMTAwLjgyYTEwLjM5LDEwLjM5LDAsMSwwLTMtMTQuMzhBMTAuMzksMTAuMzksMCwwLDAsNDAuNTgsMTAwLjgyWiIgZmlsbD0iIzAwMzNhZCIvPjxwYXRoIGQ9Ik01NS45MywxNjFhMTIuNjIsMTIuNjIsMCwxLDAsMTEuODgsMTMuMzFBMTIuNjIsMTIuNjIsMCwwLDAsNTUuOTMsMTYxWiIgZmlsbD0iIzAwMzNhZCIvPjxwYXRoIGQ9Ik00MiwyNDUuNzJhMTAuMzksMTAuMzksMCwxLDAsMTMuOTUsNC42QTEwLjM3LDEwLjM3LDAsMCwwLDQyLDI0NS43MloiIGZpbGw9IiMwMDMzYWQiLz48cGF0aCBkPSJNOTEsMTM0Ljg5YTE0Ljg0LDE0Ljg0LDAsMSwwLTQuMjctMjAuNTVBMTQuODMsMTQuODMsMCwwLDAsOTEsMTM0Ljg5WiIgZmlsbD0iIzAwMzNhZCIvPjxwYXRoIGQ9Ik0yNDYuNDcsNjkuMWExMi42MiwxMi42MiwwLDEsMC0zLjYzLTE3LjQ3QTEyLjYxLDEyLjYxLDAsMCwwLDI0Ni40Nyw2OS4xWiIgZmlsbD0iIzAwMzNhZCIvPjxwYXRoIGQ9Ik0yNzIuMzUsMjQuNTdBOC4xNiw4LjE2LDAsMSwwLDI3MCwxMy4yNiw4LjE2LDguMTYsMCwwLDAsMjcyLjM1LDI0LjU3WiIgZmlsbD0iIzAwMzNhZCIvPjxwYXRoIGQ9Ik0yNDguNDUsMTQ3LjkxYTI1LjI1LDI1LjI1LDAsMCwwLTIuODcsNTAuNDJjLjQ5LDAsMSwwLDEuNDUsMGEyNS4yNSwyNS4yNSwwLDAsMCwxLjQyLTUwLjQ2WiIgZmlsbD0iIzAwMzNhZCIvPjxwYXRoIGQ9Ik0xMzUuMDgsMTMzLjE0QTI1LjEyLDI1LjEyLDAsMCwwLDE1Ny42NCwxNDdhMjUuMjUsMjUuMjUsMCwwLDAsMjIuNTQtMzYuNjIsMjUuMjUsMjUuMjUsMCwxLDAtNDUuMSwyMi43M1oiIGZpbGw9IiMwMDMzYWQiLz48cGF0aCBkPSJNMzMzLDEwMC43OWExMC4zOSwxMC4zOSwwLDEsMC0xNC00LjZBMTAuNCwxMC40LDAsMCwwLDMzMywxMDAuNzlaIiBmaWxsPSIjMDAzM2FkIi8+PHBhdGggZD0iTTI2OSwxMDguODNhMTQuODQsMTQuODQsMCwxLDAsMTkuOTQsNi41OEExNC44NiwxNC44NiwwLDAsMCwyNjksMTA4LjgzWiIgZmlsbD0iIzAwMzNhZCIvPjxwYXRoIGQ9Ik0xODYuNTUsMjAuNzZhMTAuMzksMTAuMzksMCwxLDAtOS43OS0xMUExMC4zOCwxMC4zOCwwLDAsMCwxODYuNTUsMjAuNzZaIiBmaWxsPSIjMDAzM2FkIi8+PHBhdGggZD0iTTE4Ni40Myw4Ni4xM2ExNC44NCwxNC44NCwwLDEsMC0xNC0xNS42NkExNC44NCwxNC44NCwwLDAsMCwxODYuNDMsODYuMTNaIiBmaWxsPSIjMDAzM2FkIi8+PHBhdGggZD0iTTEwNiwyMzcuNjhhMTQuODQsMTQuODQsMCwxLDAtMTkuOTMtNi41OEExNC44NSwxNC44NSwwLDAsMCwxMDYsMjM3LjY4WiIgZmlsbD0iIzAwMzNhZCIvPjxwYXRoIGQ9Ik0xOTYsMTA3Ljc5YTI1LjIyLDI1LjIyLDAsMSwwLDIxLjE0LTExLjQxQTI1LjI4LDI1LjI4LDAsMCwwLDE5NiwxMDcuNzlaIiBmaWxsPSIjMDAzM2FkIi8+PHBhdGggZD0iTTIzOS45MiwyMTMuMzdhMjUuMjYsMjUuMjYsMCwxLDAtMTEuMTgsMzMuOTFBMjUuMTEsMjUuMTEsMCwwLDAsMjM5LjkyLDIxMy4zN1oiIGZpbGw9IiMwMDMzYWQiLz48cGF0aCBkPSJNMjg0LDIxMS42MmExNC44NCwxNC44NCwwLDEsMCw0LjI3LDIwLjU1QTE0Ljg0LDE0Ljg0LDAsMCwwLDI4NCwyMTEuNjJaIiBmaWxsPSIjMDAzM2FkIi8+PHBhdGggZD0iTTMzMi4zOCwxNzMuNjhhMTIuNjIsMTIuNjIsMCwxLDAtMTMuMzEsMTEuODhBMTIuNjIsMTIuNjIsMCwwLDAsMzMyLjM4LDE3My42OFoiIGZpbGw9IiMwMDMzYWQiLz48cGF0aCBkPSJNMzY3LjMsMTY0LjcxYTguMTYsOC4xNiwwLDEsMCw3LjY5LDguNjFBOC4xNyw4LjE3LDAsMCwwLDM2Ny4zLDE2NC43MVoiIGZpbGw9IiMwMDMzYWQiLz48cGF0aCBkPSJNMzM0LjQyLDI0NS42OGExMC4zOSwxMC4zOSwwLDEsMCwzLDE0LjM5QTEwLjM5LDEwLjM5LDAsMCwwLDMzNC40MiwyNDUuNjhaIiBmaWxsPSIjMDAzM2FkIi8+PHBhdGggZD0iTTEwMi42NSwzMjEuOTRhOC4xNiw4LjE2LDAsMSwwLDIuMzQsMTEuM0E4LjE3LDguMTcsMCwwLDAsMTAyLjY1LDMyMS45NFoiIGZpbGw9IiMwMDMzYWQiLz48cGF0aCBkPSJNMjczLjgzLDMyMS4wOGE4LjE2LDguMTYsMCwxLDAsMTEsMy42MkE4LjE2LDguMTYsMCwwLDAsMjczLjgzLDMyMS4wOFoiIGZpbGw9IiMwMDMzYWQiLz48cGF0aCBkPSJNMTc5LDIzOC43MWEyNS4yNSwyNS4yNSwwLDEsMC0yMS4xNCwxMS40MUEyNS4xLDI1LjEsMCwwLDAsMTc5LDIzOC43MVoiIGZpbGw9IiMwMDMzYWQiLz48cGF0aCBkPSJNMTI4LjUzLDI3Ny40MWExMi42MiwxMi42MiwwLDEsMCwzLjYzLDE3LjQ3QTEyLjYyLDEyLjYyLDAsMCwwLDEyOC41MywyNzcuNDFaIiBmaWxsPSIjMDAzM2FkIi8+PHBhdGggZD0iTTE4Ny4zOCwzMjUuNzRhMTAuMzksMTAuMzksMCwxLDAsOS43OCwxMUExMC4zOSwxMC4zOSwwLDAsMCwxODcuMzgsMzI1Ljc0WiIgZmlsbD0iIzAwMzNhZCIvPjxwYXRoIGQ9Ik0xODcuNDksMjYwLjM3YTE0Ljg0LDE0Ljg0LDAsMSwwLDE0LDE1LjY3QTE0Ljg1LDE0Ljg1LDAsMCwwLDE4Ny40OSwyNjAuMzdaIiBmaWxsPSIjMDAzM2FkIi8+PHBhdGggZD0iTTI0OC4yMSwyNzYuNGExMi42MiwxMi42MiwwLDEsMCwxNyw1LjU5QTEyLjYyLDEyLjYyLDAsMCwwLDI0OC4yMSwyNzYuNFoiIGZpbGw9IiMwMDMzYWQiLz48L2c+PC9nPjwvc3ZnPg==
|]

platformSVG :: String
platformSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="server" class="svg-inline--fa fa-server fa-w-16" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path fill="currentColor" d="M480 160H32c-17.673 0-32-14.327-32-32V64c0-17.673 14.327-32 32-32h448c17.673 0 32 14.327 32 32v64c0 17.673-14.327 32-32 32zm-48-88c-13.255 0-24 10.745-24 24s10.745 24 24 24 24-10.745 24-24-10.745-24-24-24zm-64 0c-13.255 0-24 10.745-24 24s10.745 24 24 24 24-10.745 24-24-10.745-24-24-24zm112 248H32c-17.673 0-32-14.327-32-32v-64c0-17.673 14.327-32 32-32h448c17.673 0 32 14.327 32 32v64c0 17.673-14.327 32-32 32zm-48-88c-13.255 0-24 10.745-24 24s10.745 24 24 24 24-10.745 24-24-10.745-24-24-24zm-64 0c-13.255 0-24 10.745-24 24s10.745 24 24 24 24-10.745 24-24-10.745-24-24-24zm112 248H32c-17.673 0-32-14.327-32-32v-64c0-17.673 14.327-32 32-32h448c17.673 0 32 14.327 32 32v64c0 17.673-14.327 32-32 32zm-48-88c-13.255 0-24 10.745-24 24s10.745 24 24 24 24-10.745 24-24-10.745-24-24-24zm-64 0c-13.255 0-24 10.745-24 24s10.745 24 24 24 24-10.745 24-24-10.745-24-24-24z"></path></svg>
|]

cpuSVG :: String
cpuSVG = [s|
<svg aria-hidden="true" focusable="false" data-prefix="fas" data-icon="microchip" class="svg-inline--fa fa-microchip fa-w-16" role="img" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path fill="currentColor" d="M416 48v416c0 26.51-21.49 48-48 48H144c-26.51 0-48-21.49-48-48V48c0-26.51 21.49-48 48-48h224c26.51 0 48 21.49 48 48zm96 58v12a6 6 0 0 1-6 6h-18v6a6 6 0 0 1-6 6h-42V88h42a6 6 0 0 1 6 6v6h18a6 6 0 0 1 6 6zm0 96v12a6 6 0 0 1-6 6h-18v6a6 6 0 0 1-6 6h-42v-48h42a6 6 0 0 1 6 6v6h18a6 6 0 0 1 6 6zm0 96v12a6 6 0 0 1-6 6h-18v6a6 6 0 0 1-6 6h-42v-48h42a6 6 0 0 1 6 6v6h18a6 6 0 0 1 6 6zm0 96v12a6 6 0 0 1-6 6h-18v6a6 6 0 0 1-6 6h-42v-48h42a6 6 0 0 1 6 6v6h18a6 6 0 0 1 6 6zM30 376h42v48H30a6 6 0 0 1-6-6v-6H6a6 6 0 0 1-6-6v-12a6 6 0 0 1 6-6h18v-6a6 6 0 0 1 6-6zm0-96h42v48H30a6 6 0 0 1-6-6v-6H6a6 6 0 0 1-6-6v-12a6 6 0 0 1 6-6h18v-6a6 6 0 0 1 6-6zm0-96h42v48H30a6 6 0 0 1-6-6v-6H6a6 6 0 0 1-6-6v-12a6 6 0 0 1 6-6h18v-6a6 6 0 0 1 6-6zm0-96h42v48H30a6 6 0 0 1-6-6v-6H6a6 6 0 0 1-6-6v-12a6 6 0 0 1 6-6h18v-6a6 6 0 0 1 6-6z"></path></svg>
|]
