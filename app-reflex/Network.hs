{-# LANGUAGE RecursiveDo, FlexibleContexts, TypeFamilies, RecordWildCards #-}

module Network where

import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Control.Monad.Fix (MonadFix)
import Reflex

import Board
import Game

newGame10 :: IO Flatris
newGame10 = newGame (10, 10)

data FlatrisMove = MoveLeft | MoveRight | MoveUp | MoveDown deriving Show

data FlatrisInputs t = FlatrisInputs
  { fiGame :: Flatris             -- ^ Initial game state
  , fiReset :: Event t ()         -- ^ Start new game
  , fiTick :: Event t UTCTime     -- ^ Regular update event
  , fiRotate :: Event t Bool      -- ^ Rotation direction
  , fiDrop :: Event t ()          -- ^ Place piece at current position
  , fiHover :: Event t (Int, Int) -- ^ Board coords of mouse hover
  , fiPush :: Event t FlatrisMove -- ^ Keyboard move
  }

data FlatrisOutputs t = FlatrisOutputs
  { foGame :: Dynamic t Flatris       -- ^ Current game state
  , foHover :: Event t BlockedBoard   -- ^ Hovering piece to show
  , foMoveCoord :: Event t (Int, Int) -- ^ Board coords of hover piece
  }

flatrisNetwork :: (Reflex t, MonadFix m, MonadHold t m) => FlatrisInputs t -> m (FlatrisOutputs t)
flatrisNetwork FlatrisInputs{..} = do
  littleChaffEv <- sometimes 30 fiTick
  bigChaffEv <- only 5 fiTick

  now <- hold (posixSecondsToUTCTime 0) fiTick

  rec
    let placed = attachWith placePiece (current game) placeCoord
    placedB <- hold id (snd <$> placed)

    let gameEv = leftmost [ rotatePiece <$> fiRotate
                          , tag placedB fiDrop
                          , resetGame <$> tag now fiReset
                          , happenEv]
        chaffEv = leftmost [ dropChaffCheck <$ littleChaffEv
                           , dropChaffPiece <$ bigChaffEv
                           ]
        happenEv = whilePlaying game chaffEv

    game <- foldDyn ($) fiGame gameEv

    (hoverCoord, placeCoord) <- makeHoverCoord (() <$ updated game) fiHover fiPush

  let hoverEv = fst <$> attachPromptlyDynWith placePiece game placeCoord

  return $ FlatrisOutputs game hoverEv hoverCoord

-- | Returns the position of the hovering piece in two different ways.
-- First is the hover position used to update the element on screen.
-- Second is the hover position used to place a piece on the board.
makeHoverCoord :: (Reflex t, MonadFix m, MonadHold t m)
               => Event t ()
               -> Event t (Int, Int)
               -> Event t FlatrisMove
               -> m (Event t (Int, Int), Event t (Int, Int))
makeHoverCoord game hover pushed = do
  rec
    let hoverGame = leftmost [ attachWith pushCoord hoverState pushed
                             , tag hoverState game ]
        hoverBoth = leftmost [ hover,  hoverGame]
    hoverState <- hold (-4,-4) hoverBoth
  return (hoverGame, hoverBoth)

pushCoord :: (Int, Int) -> FlatrisMove -> (Int, Int)
pushCoord (i, j) m = (i + x, y + j)
  where (x, y) = moveDelta m

moveDelta :: FlatrisMove -> (Int, Int)
moveDelta MoveLeft = (-1, 0)
moveDelta MoveRight = (1, 0)
moveDelta MoveUp = (0, -1)
moveDelta MoveDown = (0, 1)

whilePlaying :: Reflex t => Dynamic t Flatris -> Event t a -> Event t a
whilePlaying game = gate (current (isPlaying <$> game))

-- repeat something on an interval
sometimes :: (Reflex t, MonadFix m, MonadHold t m) => NominalDiffTime -> Event t UTCTime -> m (Event t ())
sometimes interval tick = do
  let isTime (Just l) now = diffUTCTime now l > interval
      isTime Nothing _ = True
  rec
    lastTick <- hold Nothing (Just <$> tick')
    let tick' = fmap snd . ffilter (uncurry isTime) . attach lastTick $ tick

  dropFirst (() <$ tick')

dropFirst :: (Reflex t, MonadHold t m) => Event t a -> m (Event t a)
dropFirst ev = do
  h <- hold False (True <$ ev)
  return . fmap snd . ffilter fst . attach h $ ev

-- limit number of events fired
only :: (Reflex t, MonadFix m, MonadHold t m) => Int -> Event t a -> m (Event t ())
only num ev = fmap (const ()) . ffilter (<= num) . updated <$> count ev

toMaybe :: Bool -> b -> Maybe b
toMaybe False _ = Nothing
toMaybe True b  = Just b
