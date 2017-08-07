module BoardDiagram where

import Diagrams.Prelude as D hiding (Dynamic(..))
import Diagrams.Backend.Reflex as DR
import Reflex
import Reflex.Dom as R

import Control.Monad (void)
import Data.Map (Map)
import Data.Text (Text, pack)
import Data.Array

import Board
import Game
import Display

squareSize :: Int
squareSize = 44

boardReflex :: MonadWidget t m => Dynamic t Flatris -> m (Event t (P2 Double))
boardReflex gameDyn = switchPromptly never <$> fmap diaMousemovePos =<< dyn svgDyn
  where svgDyn = boardDia . view board <$> gameDyn

boardDia :: MonadWidget t m => Board -> m (DiaEv t Any)
boardDia b = reflexDia opts (boardDiagram b)
  where opts = boardOpts "board-svg" (bounds b)

floatDia :: MonadWidget t m => BlockedBoard -> m ()
floatDia b = void . reflexDia opts . floatDiagram $ b
  where opts = boardOpts "float-svg" (bounds b)

wellDia :: MonadWidget t m => Flatris -> m ()
wellDia fl = void $ reflexDia opts (boardDiagram well)
  where
    well = fl ^. nextPiece . to expandShapeBoard
    opts = boardOpts "well-svg" (bounds well)

boardOpts :: Text -> ((Int, Int), (Int, Int)) -> Options ReflexSvg V2 Double
boardOpts cls (_, s) = def & sizeSpec .~ boardDims s &
                       svgAttributes .~ boardAttrs cls s

boardDims :: Num n => (Int, Int) -> SizeSpec V2 n
boardDims (w, h) = dims2D (s w) (s h)
  where s x = fromIntegral (x * squareSize)

boardAttrs :: Text -> (Int, Int) -> Map Text Text
boardAttrs cls (w, h) = "class" =: cls <> "width" =: s w <> "height" =: s h
  where s = pack . show . (* squareSize)

pixelToBoardCoord :: (Int, Int) -> (Int, Int)
pixelToBoardCoord (x, y) = (s x, s y)
  where s n = floor (fromIntegral n / ss')
        ss' = fromIntegral squareSize :: Float

boardCoordToPixel :: (Int, Int) -> (Int, Int)
boardCoordToPixel (x, y) = (s x, s y)
  where s = (* squareSize)
