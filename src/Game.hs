{-# LANGUAGE RecordWildCards #-}

module Game
  ( Flatris, GameState(..)
  , gameState, isPlaying, board, nextPiece, startTime, score
  , newGame, newGame'
  , rotatePiece
  , placePiece
  , resetGame
  , dropChaffPiece, dropChaffCheck
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
placePiece Flatris{..} (x, y) = (piece, action)
  where
    mark = intersection _board x y
    piece = mark _currentPiece
    action | isBlocked piece = id
           | otherwise = elim . useNextPiece
    elim = eliminateAndUpdate . over board (place x y _currentPiece)

eliminateAndUpdate :: Flatris -> Flatris
eliminateAndUpdate fl = updateGameState . (board .~ board') . addScore sc sr $ fl
  where (sc, sr, board') = eliminate (fl ^. board)

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

resetGame :: UTCTime -> Flatris -> Flatris
resetGame now = useNextPiece . useNextPiece . (over board clearBoard) .
            (gameState .~ Playing) . (score .~ 0) . (startTime .~ now)

-- Add a chaff dot and check whether it sunk the player, or cleared a line
dropChaffCheck :: Flatris -> Flatris
dropChaffCheck = eliminateAndUpdate . dropChaff

-- Put dot(s) down randomly to mess with the player
dropChaff :: Flatris -> Flatris
dropChaff fl = put (randomPiecePos (fl ^. board) (fl ^. pieceGen)) fl
  where
    put (Just (((x, y), p), g)) =
      (over board (putSingle x y (Just p))) . (pieceGen .~ g)
    put Nothing = id

-- Put a whole piece somewhere to mess with the player
dropChaffPiece :: Flatris -> Flatris
dropChaffPiece fl = (over board (place x y b)) . (pieceGen .~ g) $ fl
  where
    (((x, y), b), g) = randomShapePos (fl ^. board) (fl ^. pieceGen)

formatDiffTime :: NominalDiffTime -> Text
formatDiffTime t = T.pack $ mconcat [display m, ":", display s]
  where
    m = fromInteger $ floor (t / 60.0)
    s = fromInteger (floor t) - m * 60
    display = pad . show :: Int -> String
    pad a | length a == 0 = "00"
          | length a == 1 = ('0':a)
          | otherwise = a
