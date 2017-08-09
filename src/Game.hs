{-# LANGUAGE RecordWildCards #-}

module Game
  ( Flatris, GameState(..)
  , gameState, isPlaying, board, nextPiece, startTime, score
  , newGame, newGame'
  , rotatePiece
  , placePiece
  , resetGame
  , dropChaffPiece, dropChaff
  , clockTextMaybe
  ) where

import System.Random (StdGen, mkStdGen, random, randomR)
import Data.Time.Clock (getCurrentTime, UTCTime, NominalDiffTime, diffUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Control.Lens

import Data.Text (Text)
import qualified Data.Text as T

import Board

data GameState = Playing | Lost | Won deriving (Show, Eq)

data Flatris = Flatris { _gameState :: GameState
                       , _board :: Board
                       , _currentPiece :: Board
                       , _nextPiece :: Board
                       , _startTime :: UTCTime
                       , _score :: Int
                       , _pieceGen :: StdGen
                       } deriving Show

makeLenses ''Flatris

newGame :: (Int, Int) -> IO Flatris
newGame size = do
  now <- getCurrentTime
  return $ newGame' size now (round $ utcTimeToPOSIXSeconds now)

newGame' :: (Int, Int) -> UTCTime -> Int -> Flatris
newGame' (w,h) t seed = useNextPiece $ useNextPiece blank
  where blank = Flatris { _gameState = Playing
                        , _board = emptyBoard Nothing w h
                        , _currentPiece = undefined
                        , _nextPiece = undefined
                        , _startTime = t
                        , _score = 0
                        , _pieceGen = mkStdGen seed
                        }

useNextPiece :: Flatris -> Flatris
useNextPiece = randomRotate . choosePiece . switchPiece

switchPiece :: Flatris -> Flatris
-- copy nextPiece to currentPiece
switchPiece fl = currentPiece .~ (fl ^. nextPiece) $ fl

choosePiece :: Flatris -> Flatris
choosePiece fl = (nextPiece .~ p') . (pieceGen .~ g') $ fl
  where
    p' =  shapeBoard p
    (p, g') = random (fl ^. pieceGen)

randomRotate :: Flatris -> Flatris
randomRotate fl = (over nextPiece (rotateN r)) . (pieceGen .~ g') $ fl
  where
    (r, g') = randomR (0,3) (fl ^. pieceGen)

rotatePiece :: Bool -> Flatris -> Flatris
rotatePiece dir = over currentPiece (rotate dir)

placePiece :: Flatris -> (Int, Int) -> (BlockedBoard, Flatris -> Flatris)
placePiece Flatris{..} (x, y) = (piece, result)
  where
    mark = intersection _board x y
    piece = mark _currentPiece
    (sc, sr, board') = eliminate $ place x y _currentPiece _board
    result | isBlocked piece = id
           | otherwise = updateGameState . (board .~ board') . addScore sc sr . useNextPiece

addScore :: Int -> Int -> Flatris -> Flatris
addScore cols rows = over score (+ bonus)
  where bonus = (2 ^ cols) * (2 ^ rows)

updateGameState :: Flatris -> Flatris
updateGameState fl = gameState .~ state $ fl
  where state = checkState (fl ^. board) (fl ^. currentPiece)

checkState :: Board -> Board -> GameState
checkState b p | isEmpty b = Won
               | sunk b p  = Lost
               | otherwise = Playing

clockText :: Flatris -> UTCTime -> Text
clockText fl now = formatDiffTime (diffUTCTime now (fl ^. startTime))

clockTextMaybe :: Flatris -> UTCTime -> Maybe Text
clockTextMaybe fl | isPlaying fl = Just . clockText fl
                  | otherwise    = const Nothing

isPlaying :: Flatris -> Bool
isPlaying = (== Playing) . view gameState

-- Clear everything except the RNG and time ... hmm
resetGame :: Flatris -> Flatris
resetGame = useNextPiece . useNextPiece . (over board clearBoard) .
            (gameState .~ Playing) . (score .~ 0)

-- Put dot(s) down randomly to mess with the player
dropChaff :: Flatris -> Flatris
dropChaff fl = (over board (putSingle x y (Just p))) . (pieceGen .~ g) $ fl
  where
    (((x, y), p), g) = randomPiecePos (fl ^. board) (fl ^. pieceGen)

-- Put a whole piece somewhere to mess with the player
dropChaffPiece :: Flatris -> Flatris
dropChaffPiece fl = (over board (place x y b)) . (pieceGen .~ g) $ fl
  where
    (((x, y), b), g) = randomShapePos (fl ^. board) (fl ^. pieceGen)

{-
import Data.Text.Format
-- problem with reflex double-conversion library
formatDiffTime :: NominalDiffTime -> Text
formatDiffTime t = format (pad m) (pad s) "{}:{}"
  where
    pad = left 2 '0'
    m = floor (t / 60)
    s = t - m * 60
-}

formatDiffTime :: NominalDiffTime -> Text
formatDiffTime t = T.pack $ mconcat [display m, ":", display s]
  where
    m = fromInteger $ floor (t / 60.0)
    s = fromInteger (floor t) - m * 60
    display = pad . show :: Int -> String
    pad a | length a == 0 = "00"
          | length a == 1 = ('0':a)
          | otherwise = a
