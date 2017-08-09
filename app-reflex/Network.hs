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
  { fiGame :: Flatris
  , fiReset :: Event t ()
  , fiTick :: Event t UTCTime
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
  littleChaffEv <- sometimes 30 fiTick
  bigChaffEv <- only 5 fiTick

  now <- hold (posixSecondsToUTCTime 0) fiTick

  -- fixme: this would be slightly simpler if placePiece were cut in half
  rec
    let placed = attachWith placePiece (current game) hoverCoord
    placedB <- hold id (snd <$> placed)

    let gameEv = leftmost [ rotatePiece <$> fiRotate
                          , tag placedB fiDrop
                          , resetGame <$> tag now fiReset
                          , happenEv]
        chaffEv = leftmost [ dropChaff <$ littleChaffEv
                           , dropChaffPiece <$ bigChaffEv
                           ]
        happenEv = whilePlaying game chaffEv

    game <- foldDyn ($) fiGame gameEv

    hoverCoord <- makeHoverCoord (() <$ updated game) fiHover fiPush

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
