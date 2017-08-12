module BoardWidget
  ( boardSvg
  , floatSvg
  , wellSvg
  , squareSize
  , pixelToBoardCoord
  , boardCoordToPixel
  ) where

import Reflex
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.Svg

import Control.Monad (void)
import Data.Map (Map)
import Data.Text (Text, pack)
import Data.Array
import Data.Monoid ((<>))
import Control.Lens

import Game
import Board
import BoardUtil (tshow)

squareSize :: Int
squareSize = 44

pixelToBoardCoord :: (Int, Int) -> (Int, Int)
pixelToBoardCoord (x, y) = (s x, s y)
  where s n = floor (fromIntegral n / ss')
        ss' = fromIntegral squareSize :: Float

boardCoordToPixel :: (Int, Int) -> (Int, Int)
boardCoordToPixel (x, y) = (s x, s y)
  where s = (* squareSize)

-- Creates an svg container of the given class with appropriate
-- dimensions and viewBox.
boardElem :: MonadWidget t m
          => (Array BIx e -> Map Text Text)
          -> (e -> Text)
          -> Dynamic t (Array BIx e) -> m ()
boardElem attrs f boardDyn = svgDynAttr "svg" (attrs <$> boardDyn) $
                             svgClass "g" "grid" $
                             (grid f boardDyn)

svgAttrs :: Text -> Array BIx e -> Map Text Text
svgAttrs cls b = svgSizeAttrs cls w h
  where
    (_, (w, h)) = bounds b

svgSizeAttrs :: Text -> Int -> Int -> Map Text Text
svgSizeAttrs cls w h = "class" =: cls <> "viewBox" =: viewBox <> "width" =: tshow w' <> "height" =: tshow h'
  where
    w' = w * squareSize
    h' = h * squareSize
    viewBox = pack . unwords . map show $ [0, 0, w', h']

grid :: MonadWidget t m => (e -> Text) -> Dynamic t (Array BIx e) -> m ()
grid f boardDyn = void $ dyn (contents <$> boardDyn)
  where contents = mapM_ (boardSquare f) . assocs

boardSvg :: MonadWidget t m => Dynamic t Flatris -> m ()
boardSvg = boardElem (svgAttrs "board-svg") boardSquareClass . fmap (view board)

floatSvg :: MonadWidget t m => BlockedBoard -> m ()
floatSvg = boardElem attrs blockedBoardSquareClass . pure
  where attrs = const $ svgSizeAttrs "float-svg" 4 4

wellSvg :: MonadWidget t m => Dynamic t Flatris -> m ()
wellSvg = boardElem (svgAttrs "well-svg") boardSquareClass . nextPieceDyn
  where nextPieceDyn = fmap (view (nextPiece . to expandShapeBoard))

boardSquare :: MonadWidget t m => (s -> Text) -> ((Int, Int), s) -> m ()
boardSquare f ((i, j), p) = svgAttr "rect" attrs blank
  where
    attrs = ("class" =: cls <> "width" =: "40" <> "height" =: "40" <>
              "x" =: tshow x <> "y" =: tshow y)
    x = (i - 1) * squareSize
    y = (j - 1) * squareSize
    cls = "square " <> f p

-- mapping contents of square to css class
boardSquareClass :: Maybe Piece -> Text
boardSquareClass = maybe "empty" pieceClass

-- mapping contents of square to css class
blockedBoardSquareClass :: Either Bool Piece -> Text
blockedBoardSquareClass = either bad pieceClass
  where bad True = "blocked"
        bad False = "empty"

pieceClass :: Piece -> Text
pieceClass l = "piece-" <> pieceLetter l
  where
    pieceLetter OPiece = "o"
    pieceLetter IPiece = "i"
    pieceLetter LPiece = "l"
    pieceLetter JPiece = "j"
    pieceLetter TPiece = "t"
    pieceLetter SPiece = "s"
    pieceLetter ZPiece = "z"
