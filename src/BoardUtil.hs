module BoardUtil where

import Data.List.Extra (trim, elemIndex)
import Data.Maybe (catMaybes, listToMaybe)

import Board

import Data.Array (array, Array, elems)

pieceLetter :: Piece -> Char
pieceLetter OPiece = 'O'
pieceLetter IPiece = 'I'
pieceLetter LPiece = 'L'
pieceLetter JPiece = 'J'
pieceLetter TPiece = 'T'
pieceLetter SPiece = 'S'
pieceLetter ZPiece = 'Z'

readPiece :: Char -> Maybe Piece
readPiece c = lookup c cs
  where cs = [(pieceLetter p, p) | p <- [minBound .. maxBound]]

showPiece :: Maybe Piece -> Char
showPiece = maybe '.' pieceLetter

printBoard :: Board -> IO ()
printBoard = putStr . showBoard

showBoard :: Board -> String
showBoard = showBoard' showPiece

showBoard' :: (a -> Char) -> Array (Int, Int) a -> String
showBoard' s = unlines . boardRows . fmap s

readBoard :: String -> Board
readBoard = readBoard' readPiece . lines

readBoard' :: (Char -> a) -> [String] -> Array (Int, Int) a
readBoard' f = fmap f . make
  where
    make ls = array bounds (zip (rowIndex bounds) es)
      where
        bounds = ((1,1),(w,h))
        w = if null ls then 0 else length (head ls)
        h = length ls
        pad = take w . (++ repeat '.')
        es = concatMap pad ls

showBlockedBoard :: BlockedBoard -> String
showBlockedBoard = showBoard' (either blocked pieceLetter)
  where
    blocked True = 'X'
    blocked False = '.'

readBlockedPiece :: Char -> Either Bool Piece
readBlockedPiece 'X' = Left True
readBlockedPiece  c  = maybe (Left False) Right (readPiece c)

readTwoBoards :: [String] -> (Board, Board)
readTwoBoards = readTwoBoards' readPiece readPiece

readTwoBoards' :: (Char -> p1) -> (Char -> p2) -> [String]
               -> (Array (Int, Int) p1, Array (Int, Int) p2)
readTwoBoards' r1 r2 ls = (readBoard' r1 . readRows $ b1, readBoard' r2 . readRows $ b2)
  where
    (b1, b2) = unzip $ map (splitAt k) ls
    readRows = filter (not . null) . map trim
    Just k = elemIndex ' ' (head ls)

readBoardAndBlocked :: [String] -> (Board, BlockedBoard)
readBoardAndBlocked = readTwoBoards' readPiece readBlockedPiece

invertBoard :: Board -> Board
invertBoard b = fmap invert b
  where
    invert Nothing = boardShape b
    invert (Just _) = Nothing

otherThan :: Piece -> [Piece]
otherThan p = filter (/= p) [minBound .. maxBound]

boardShape :: Board -> Maybe Piece
boardShape = listToMaybe . catMaybes . elems
