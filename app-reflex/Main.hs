{-# LANGUAGE FlexibleContexts, TypeFamilies, RecordWildCards, RecursiveDo #-}

module Main where

import Reflex
import Reflex.Dom

import GHCJS.DOM.EventM (mouseOffsetXY, EventM, event, mouseClientXY, uiPageXY, mouseButton, eventTarget)
import GHCJS.DOM.Types (IsMouseEvent(..), DOMRect)
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.Node as DOM
import qualified GHCJS.DOM.Document as DOM
import GHCJS.DOM.Element (getBoundingClientRect)
import qualified GHCJS.DOM.DOMRect as DOMRect
import GHCJS.DOM.Event (getTargetUnsafe)
import qualified GHCJS.DOM.MouseEvent as MouseEvent
import qualified GHCJS.DOM.WheelEvent as WheelEvent

import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Data.Monoid ((<>))

import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as T

import Data.Map (Map)
import Control.Lens hiding ((#))

import Board
import Game
import Network
import BoardWidget
import MainWidget

main :: IO ()
main = do
  game <- newGameWithStuff
  mainWidgetFlatris $ do
    divClass "container-left" $ app game


app :: MonadWidget t m => Flatris -> m ()
app initial = mdo
  (elm, _) <- elAttr' "div" ("class" =: "game" <> "oncontextmenu" =: "return false;") $ do
    divClass "board" $ void $ boardReflex gameDyn

    divClass "right" $ do
      divClass "title" $ do
        el "h1" $ text "Flatris"

      divClass "game-state" $ display (view gameState <$> gameDyn)

      theClock gameDyn
      theScore gameDyn
      theWell gameDyn

      helpButtons

    thePiece hoverEv hoverUpdateEv

  body <- DOM.getOwnerDocumentUnsafe (_element_raw elm) >>= DOM.getBodyUnsafe
  keyEv <- wrapDomEvent body (elementOnEventName Keypress) $ do
    DOM.preventDefault
    DOM.stopPropagation
    DOM.returnValue False
    getKeyEvent

  wheelEv <- wrapDomEvent body (elementOnEventName Wheel) $ do
    DOM.preventDefault
    DOM.stopPropagation
    DOM.returnValue False
    return False

  let pixelCoordEv = domEvent Mousemove elm
  boardCoord <- fmap pixelToBoardCoord <$> offsetMouseEvent elm Mousemove
  let hoverUpdateEv = makeHoverEvent pixelCoordEv hoverCoord

  let gameInputs = FlatrisInputs
                   { fiGame = initial
                   , fiRotate = leftmost [ wheelEv
                                         , fmapMaybe keycodeRotate keyEv ]
                   , fiDrop = leftmost [ domEvent Click elm, () <$ ffilter keycodeDrop keyEv ]
                   , fiHover = boardCoord
                   , fiPush = fmapMaybe keycodeMove keyEv
                   }
  FlatrisOutputs gameDyn hoverEv hoverCoord <- flatrisNetwork gameInputs

  return ()

keycodeRotate :: Word -> Maybe Bool
-- space, semicolon, Q/E
keycodeRotate  32 = Just False
keycodeRotate  59 = Just False
keycodeRotate 101 = Just False
keycodeRotate 113 = Just True
keycodeRotate   _ = Nothing

keycodeDrop :: Word -> Bool
-- enter or F
keycodeDrop c = c == 13 || c == 102

keycodeMove :: Word -> Maybe FlatrisMove
-- HJKL keys
keycodeMove 106 = Just MoveUp
keycodeMove 107 = Just MoveDown
keycodeMove 104 = Just MoveLeft
keycodeMove 108 = Just MoveRight
-- WASD keys
keycodeMove 119 = Just MoveUp
keycodeMove 115 = Just MoveDown
keycodeMove  97 = Just MoveLeft
keycodeMove 100 = Just MoveRight
keycodeMove   _ = Nothing

-- mouse event co-ordinates relative to an element
offsetMouseEvent elm ev = wrapDomEvent (_element_raw elm) (elementOnEventName ev) mouseOffsetXY

theClock, theScore :: MonadWidget t m => Dynamic t Flatris -> m ()
theClock gameDyn = divClass "clock" $ do
  divClass "clock-label" $ text "Clock:"
  now <- liftIO getCurrentTime
  evTick <- tickLossy 1 now
  let evTime = _tickInfo_lastUTC <$> evTick
  let evClock = attachPromptlyDynWith clockTextMaybe gameDyn evTime
  divClass "clock-timer" $
    dynText =<< holdDyn "" (fmapMaybe id evClock)

theScore gameDyn = divClass "score" $ do
  divClass "score-label" $ text "Score:"
  divClass "score-value" $ display (view score <$> gameDyn)

makeHoverEvent :: Reflex t => Event t (Int, Int)
               -> Event t (Int, Int)
               -> Event t (Int, Int)
makeHoverEvent mousePos boardPos = leftmost
  [ offset <$> mousePos, pad . boardCoordToPixel <$> boardPos ]
  where
    half = squareSize `div` 2
    offset (x, y) = (x - half, y - half)
    pad (x, y) = (x + 8, y + 8) -- fixme: value from dom

thePiece :: MonadWidget t m => Event t BlockedBoard
         -> Event t (Int, Int) -> m ()
thePiece hover pos = do
  attrsDyn <- holdDyn mempty (positionClass "floating-piece" <$> pos)
  let elm b = elDynAttr "div" attrsDyn (floatDia b)
  void . dyn =<< holdDyn blank (elm <$> hover)

theWell :: MonadWidget t m => Dynamic t Flatris -> m (Event t ())
theWell gameDyn = elAttr "div" ("class" =: "well") (dyn (wellDia <$> gameDyn))

positionClass :: (Num a, Show a) => Text -> (a, a) -> Map Text Text
positionClass cls (x, y) = "class" =: cls <> "style" =: ("position: fixed; left: " <> tshow x <> "px; top: " <> tshow y <> "px;")

positionStyle :: (Num a, Show a) => (a, a) -> Map Text Text
positionStyle (x, y) = "style" =: ("position: fixed; left: " <> tshow x <> "px; top: " <> tshow y <> "px;")

githubUrl :: Text
githubUrl = "https://github.com/rvl/flatris"

helpButtons :: MonadWidget t m => m (Dynamic t Bool)
helpButtons = divClass "help-buttons" $ do
  elAttr "a" ("href" =: githubUrl <> "target" =: "_blank" <> "class" =: "button github") (text "Code")
  (e, _) <- elAttr' "button" ("class" =: "button-primary help") (text "Help")
  rec
    showHelp <- toggle False $ leftmost [ domEvent Click e, closeEv ]
    closeEv2 <- dyn . ffor showHelp $ \s -> if s then instructions else return never
    closeEv <- switchPromptly never closeEv2
  return showHelp

instructions :: MonadWidget t m => m (Event t ())
instructions = divClass "instructions" $ do
  el "h2" $ text "Instructions"
  el "p" $ text "Drop pieces coming from the well into empty spaces on board."
  el "p" $ text "Make horizontal or vertical lines to score."
  divClass "instructions-keys" $ do
    let key (k, s) = do
          elAttr "span" ("class" =: "instruction-key") $ text k
          elAttr "span" ("class" =: "instruction-symbol") $ text s
    divClass "row" $ mapM_ key [ ("Q", "↺"), ("W", "↑"), ("E", "↻") ]
    divClass "row" $ mapM_ key [ ("A", "←"), ("S", "↓"), ("D", "→"), ("F", "⏎") ]
  divClass "instructions-buttons" $ do
    button "Close"

tshow :: Show a => a -> Text
tshow = T.pack . show