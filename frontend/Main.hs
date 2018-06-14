{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE JavaScriptFFI       #-}
module Main where

import           Data.IORef
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           GHCJS.Foreign hiding (Object)
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Types
import           JavaScript.Object
import           JavaScript.Object.Internal
import           Miso hiding (preventDefault)
import           Miso.String

data PageInfo = PageInfo
  { currentPage :: !Int
  , numPages :: !Int
  , selectedPages :: !IntSet
  } deriving (Eq)

forwardPage :: PageInfo -> PageInfo
forwardPage (PageInfo i n selected)
  | i >= n = PageInfo n n selected
  | otherwise = PageInfo (i+1) n selected

backwardPage :: PageInfo -> PageInfo
backwardPage (PageInfo i n selected)
  | i <= 1 = PageInfo 1 n selected
  | otherwise = PageInfo (i - 1) n selected

-- | Model
data Model = Model
  { pageInfo :: !(Maybe PageInfo)
  } deriving (Eq)

-- | Action
data Action
  = Forward
  | Backward
  | SelectPage
  | ReadFile
  | SetPageInfo !PageInfo
  | Submit
  | NoOp

main :: IO ()
main = do
  docRef <- newIORef jsNull
  startApp
    App
    { model = Model Nothing
    , initialAction = NoOp
    , update = updateModel docRef
    , mountPoint = Nothing
    , ..
    }
  where
    events = defaultEvents
    subs = [navSub]
    view = viewModel

data RenderContext = RenderContext
  { renderCtxCanvasCtx :: !JSVal
  , renderCtxViewPort :: !JSVal
  }

instance ToJSVal RenderContext where
  toJSVal (RenderContext ctx vp) = do
    obj@(Object obj') <- create
    setProp "canvasContext" ctx obj
    setProp "viewport" vp obj
    return obj'

renderPage :: IORef JSVal -> Int -> IO ()
renderPage docRef pageNum = do
  doc <- readIORef docRef
  page <- pdfjsGetPage doc pageNum
  unscaledVp <- pdfjsGetViewport page 1
  height <- viewportHeight unscaledVp
  width <- viewportWidth unscaledVp
  viewport <-
    pdfjsGetViewport
      page
      (min
         (fromIntegral canvasHeight / fromIntegral height)
         (fromIntegral canvasWidth / fromIntegral width))
  ctx <- get2dContext =<< getElementById "pdf-canvas"
  renderCtx <- toJSVal (RenderContext ctx viewport)
  pdfjsRenderPage page renderCtx

-- | Update your model
updateModel :: IORef JSVal -> Action -> Model -> Effect Action Model
updateModel docRef ReadFile m = m <# do
  fileReaderInput <- getElementById "file-input"
  file <- getFile fileReaderInput
  reader <- newReader
  fileContent <- readAsArrayBuffer reader file
  doc <- pdfjsGetDocument fileContent
  writeIORef docRef doc
  pages <- pdfjsNumPages doc
  return (SetPageInfo (PageInfo 1 pages IntSet.empty))
updateModel docRef (SetPageInfo pageInfo@(PageInfo i _ _)) m = (m { pageInfo = Just pageInfo }) <# do
  renderPage docRef i
  return NoOp
updateModel _ Forward (Model Nothing) = noEff (Model Nothing)
updateModel docRef Forward m@(Model (Just pageInfo)) =
  let pageInfo' = forwardPage pageInfo
  in m {pageInfo = Just pageInfo'} <#
     (NoOp <$ renderPage docRef (currentPage pageInfo'))
updateModel _ Backward (Model Nothing) = noEff (Model Nothing)
updateModel docRef Backward m@(Model (Just pageInfo)) =
  let pageInfo' = backwardPage pageInfo
  in m {pageInfo = Just pageInfo'} <#
     (NoOp <$ renderPage docRef (currentPage pageInfo'))
updateModel _ SelectPage m = noEff (m {pageInfo = fmap togglePage (pageInfo m)})
updateModel _ Submit m@(Model Nothing) = noEff m
updateModel _ Submit m@(Model (Just pageInfo)) =
  m <# do
    fileInput <- getElementById "file-input"
    file <- getFile fileInput
    let pages = (ms . show . IntSet.toAscList . selectedPages) pageInfo
    NoOp <$ generatePDF file pages
updateModel _ NoOp m = noEff m

viewNav :: View Action
viewNav =
  div_
    [class_ "nav-container"]
    [ nav_
        []
        [ label_
            [class_ "button"]
            [ input_
                [ id_ "file-input"
                , type_ "file"
                , name_ "file"
                , onChange (\_ -> ReadFile)
                ]
            , "Upload PDF"
            ]
        , span_ [class_ "button", onClick Submit] ["Generate PDF"]
        , span_ [class_ "filler"] []
        , span_ [class_ "nav-title"] ["pdf-foobar"]
        ]
    ]

viewPageInfo :: Maybe PageInfo -> View a
viewPageInfo pageInfo =
  div_
    [class_ "page-info"]
    (case pageInfo of
       Nothing -> []
       Just pageInfo' ->
         [ (text . ms . show . currentPage) pageInfo'
         , "/"
         , (text . ms . show . numPages) pageInfo'
         ])

viewOverlay :: Maybe PageInfo -> View a
viewOverlay Nothing = div_ [class_ "no-file"] ["No file selected"]
viewOverlay (Just (PageInfo p _ sel)) =
  div_ [class_ (Miso.String.unwords ("overlay" : selectedClass))] []
  where
    selectedClass
      | p `IntSet.member` sel = ["selected"]
      | otherwise = []


viewCanvas :: Maybe PageInfo -> View a
viewCanvas pageInfo =
  div_
    [class_ "canvas-container"]
    [ canvas_
        [ id_ "pdf-canvas"
        , width_ (ms $ show canvasWidth)
        , height_ (ms $ show canvasHeight)
        ]
        []
    , viewOverlay pageInfo
    ]


-- | View function, with routing
viewModel :: Model -> View Action
viewModel (Model pageInfo) = view
  where
    view =
      div_
        []
        [ script_ [src_ "https://mozilla.github.io/pdf.js/build/pdf.js"] []
        , link_ [href_ "/assets/style.css", rel_ "stylesheet"]
        , viewNav
        , main_ [] [viewCanvas pageInfo, viewPageInfo pageInfo]
        ]

togglePage :: PageInfo -> PageInfo
togglePage (PageInfo p t sel)
  | p `IntSet.member` sel = PageInfo p t (IntSet.delete p sel)
  | otherwise = PageInfo p t (IntSet.insert p sel)

canvasWidth :: Int
canvasWidth = 800

canvasHeight :: Int
canvasHeight = 600

navSub :: Sub Action
navSub sink = do
  windowAddEventListener "keydown" =<<
    (asyncCallback1 $ \event -> do
       Just key <- fromJSVal =<< getProp "key" (Object event)
       case (key :: JSString) of
         "ArrowLeft" -> sink Backward
         "ArrowRight" -> sink Forward
         " " -> sink SelectPage >> preventDefault event
         _ -> pure ())
  windowAddEventListener "keyup" =<<
    (asyncCallback1 $ \event -> do
       Just key <- fromJSVal =<< getProp "key" (Object event)
       case (key :: JSString) of
         " " -> preventDefault event
         _ -> pure ())

foreign import javascript unsafe "$r = new FileReader();"
  newReader :: IO JSVal

foreign import javascript unsafe "$r = document.getElementById($1);"
  getElementById :: MisoString -> IO JSVal

foreign import javascript unsafe "$r = $1.files[0];"
  getFile :: JSVal -> IO JSVal

foreign import javascript interruptible "$1.onload = function(e) { $c(new Uint8Array(e.target.result)); }; $1.readAsArrayBuffer($2);"
  readAsArrayBuffer :: JSVal -> JSVal -> IO JSVal

foreign import javascript interruptible "pdfjsLib.getDocument($1).then($c);"
  pdfjsGetDocument :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.numPages"
  pdfjsNumPages :: JSVal -> IO Int

foreign import javascript interruptible "$1.getPage($2).then($c);"
  pdfjsGetPage :: JSVal -> Int -> IO JSVal

foreign import javascript interruptible "$1.render($2).promise.then($c);"
  pdfjsRenderPage :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$1.getViewport($2)"
  pdfjsGetViewport :: JSVal -> Double -> IO JSVal

foreign import javascript unsafe "$1.height"
  viewportHeight :: JSVal -> IO Int

foreign import javascript unsafe "$1.width"
  viewportWidth :: JSVal -> IO Int

foreign import javascript unsafe "$1.getContext(\"2d\")"
  get2dContext :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.preventDefault();"
               preventDefault :: JSVal -> IO ()

foreign import javascript unsafe "generatePDF($1, $2);"
               generatePDF :: JSVal -> JSString -> IO ()
