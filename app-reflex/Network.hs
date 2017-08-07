{-# LANGUAGE RecursiveDo, FlexibleContexts, TypeFamilies, RecordWildCards #-}

module Network where

import Control.Lens
import Control.Monad.Fix (MonadFix)
import Reflex

import Board
import Game

newGameWithStuff :: IO Flatris
newGameWithStuff = do
  game <- newGame (10, 10)
  let p1 = Board.place 2 2 tPiece
  let p2 = Board.place 4 4 lPiece
  return $ over board (p2 . p1) game

data FlatrisMove = MoveLeft | MoveRight | MoveUp | MoveDown deriving Show

data FlatrisInputs t = FlatrisInputs
  { fiGame :: Flatris
  , fiRotate :: Event t Bool
  , fiDrop :: Event t ()
  , fiHover :: Event t (Int, Int)
  , fiPush :: Event t FlatrisMove
  }

data FlatrisOutputs t = FlatrisOutputs
  { foGame :: Dynamic t Flatris
  , foHover :: Event t BlockedBoard
  , foMoveCoord :: Event t (Int, Int)
  }

flatrisNetwork :: (Reflex t, MonadFix m, MonadHold t m) => FlatrisInputs t -> m (FlatrisOutputs t)
flatrisNetwork FlatrisInputs{..} = do
  rec
    let placed = attachWith placePiece (current game) hoverCoord
    placedB <- hold id (snd <$> placed)

    let gameEv = leftmost [ rotatePiece <$> fiRotate
                          , tag placedB fiDrop
                          ]
    game <- foldDyn ($) fiGame gameEv

    hoverCoord <- makeHoverCoord (() <$ updated game) fiHover fiPush

  -- fixme: messy, maybe need to split placePiece in half
  -- this needs to be a "promptly" attachment
  let hoverEv = fst <$> attachPromptlyDynWith placePiece game (leftmost [hoverCoord, fiHover])

  return $ FlatrisOutputs game hoverEv hoverCoord

makeHoverCoord :: (Reflex t, MonadFix m, MonadHold t m)
               => Event t ()
               -> Event t (Int, Int)
               -> Event t FlatrisMove
               -> m (Event t (Int, Int))
makeHoverCoord game hover pushed = do
  rec
    hoverState <- hold (0,0) hover'
    let hover' = leftmost [ attachWith pushCoord hoverState pushed
                          , hover
                          , tag hoverState game ]
  return hover'

pushCoord :: (Int, Int) -> FlatrisMove -> (Int, Int)
pushCoord (i, j) m = (i + x, y + j)
  where (x, y) = moveDelta m

moveDelta :: FlatrisMove -> (Int, Int)
moveDelta MoveLeft = (-1, 0)
moveDelta MoveRight = (1, 0)
moveDelta MoveUp = (0, -1)
moveDelta MoveDown = (0, 1)

toMaybe :: Bool -> b -> Maybe b
toMaybe False _ = Nothing
toMaybe True b  = Just b
