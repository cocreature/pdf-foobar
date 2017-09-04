{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE JavaScriptFFI       #-}
module Main where

import           Control.Concurrent.MVar
import           Data.Aeson hiding (Object)
import           Data.IORef
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.Maybe
import           GHCJS.Foreign hiding (Object)
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal
import           GHCJS.Types
import           JavaScript.Object
import           JavaScript.Object.Internal
import           Miso hiding (preventDefault)
import           Miso.String
import           Text.Read

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
  | NoOp

main :: IO ()
main = do
  docRef <- newIORef jsNull
  startApp
    App
    { model = Model Nothing
    , initialAction = NoOp
    , update = updateModel docRef
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
renderPage docRef page = do
  doc <- readIORef docRef
  page <- pdfjsGetPage doc page
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
updateModel _ NoOp m = noEff m

css :: MisoString
css = ".overlay {\
      \  position: absolute;\
      \  left: 0;\
      \  top: 0;\
      \  width: 100%;\
      \  height: 100%;\
      \  background-color: #0009;\
      \  transition: background-color .2s;\
      \}\
      \.selected {\
      \  background-color: #0000;\
      \}\
      \.canvas-container {\
      \  position: relative;\
      \  width: 800px;\
      \  height: 600px;\
      \}\
      \"

-- | View function, with routing
viewModel :: Model -> View Action
viewModel (Model pageInfo) = view
  where
    view =
      div_
        []
        [ script_ [src_ "https://mozilla.github.io/pdf.js/build/pdf.js"] []
        , nodeHtml "style" [] [text css]
        , h1_ [] ["pdf-foobar"]
        , div_
            []
            [ input_ [id_ "file-input", type_ "file", onChange ReadFile] []
            , span_
                []
                [ span_
                    []
                    (maybe [] (return . text . ms . show . currentPage) pageInfo)
                , "/"
                , span_
                    []
                    (maybe [] (return . text . ms . show . numPages) pageInfo)
                ]
            ]
        , div_
            [class_ "canvas-container"]
            [ canvas_
                [ id_ "pdf-canvas"
                , width_ (ms $ show canvasWidth)
                , height_ (ms $ show canvasHeight)
                ]
                []
            , div_ ([class_ (Miso.String.unwords ("overlay" : selectedClass))]) []
            ]
        ]
      where
        selectedClass
          | Just (PageInfo p _ sel) <- pageInfo
          , p `IntSet.member` sel = ["selected"]
          | otherwise = []

togglePage :: PageInfo -> PageInfo
togglePage (PageInfo p t sel)
  | p `IntSet.member` sel = PageInfo p t (IntSet.delete p sel)
  | otherwise = PageInfo p t (IntSet.insert p sel)

canvasWidth :: Int
canvasWidth = 800

canvasHeight :: Int
canvasHeight = 600

foreign import javascript unsafe "$r = new FileReader();"
  newReader :: IO JSVal

foreign import javascript unsafe "$r = document.getElementById($1);"
  getElementById :: MisoString -> IO JSVal

foreign import javascript unsafe "$r = $1.files[0];"
  getFile :: JSVal -> IO JSVal

foreign import javascript interruptible "$1.onload = function(e) { $c(new Uint8Array(e.target.result)); }; $1.readAsArrayBuffer($2);"
  readAsArrayBuffer :: JSVal -> JSVal -> IO JSVal

foreign import javascript interruptible "PDFJS.getDocument($1).then($c);"
  pdfjsGetDocument :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.numPages"
  pdfjsNumPages :: JSVal -> IO Int

foreign import javascript unsafe "window.addEventListener($1, $2);"
  windowAddEventListener :: JSString -> Callback (JSVal -> IO JSVal) -> IO ()

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

onChange :: action -> Attribute action
onChange r = on "change" emptyDecoder (const r)

navSub :: Sub Action model
navSub _ sink = do
  windowAddEventListener "keypress" =<<
    (syncCallback1' $ \event -> do
       Just key <- fromJSVal =<< getProp "key" (Object event)
       case (key :: JSString) of
         "ArrowLeft" -> sink Backward
         "ArrowRight" -> sink Forward
         " " -> sink SelectPage
         _ -> return ()
       return jsTrue)
  windowAddEventListener "keyup" =<<
    (syncCallback1' $ \event -> do
       Just key <- fromJSVal =<< getProp "key" (Object event)
       case (key :: JSString) of
         " " -> preventDefault event >> return jsFalse
         _ -> return jsTrue)

foreign import javascript unsafe "$1.preventDefault();"
               preventDefault :: JSVal -> IO ()
