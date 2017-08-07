module Board where

import Data.Array
import System.Random
import Data.Maybe (isJust, isNothing)
import Data.List (nub)

type BIx = (Int, Int)
type Board = Array BIx (Maybe Piece)
type BlockedBoard = Array BIx (Either Bool Piece)

data Piece = OPiece | IPiece | LPiece | JPiece | TPiece | SPiece | ZPiece
  deriving (Show, Eq, Enum, Bounded, Ord, Ix)

pieceShape :: Piece -> [String]
pieceShape OPiece = [ "OO"
                    , "OO"   ]
pieceShape IPiece = [ "IIII" ]
pieceShape LPiece = [ "LLL"
                    , "L  "  ]
pieceShape JPiece = [ "J  "
                    , "JJJ"  ]
pieceShape TPiece = [ " T "
                    , "TTT"  ]
pieceShape SPiece = [ " SS"
                    , "SS "  ]
pieceShape ZPiece = reverse (pieceShape SPiece)

shapeBoard :: Piece -> Board
shapeBoard = makeArray . makePiece
  where
    makePiece p = map (map conv) $ pieceShape p
      where conv ' ' = Nothing
            conv _ = Just p

    makeArray :: [[Maybe Piece]] -> Board
    makeArray rows = array b (zip ix (concat rows))
      where
        ix = rowIndex b
        b = boardBounds w h
        w = length (head rows)
        h = length rows

oPiece, iPiece, lPiece, jPiece, tPiece, sPiece, zPiece :: Board
oPiece = shapeBoard OPiece
iPiece = shapeBoard IPiece
lPiece = shapeBoard LPiece
jPiece = shapeBoard JPiece
tPiece = shapeBoard TPiece
sPiece = shapeBoard SPiece
zPiece = shapeBoard ZPiece

expandShapeBoard :: Board -> Board
expandShapeBoard = expandBoard Nothing 4 4

expandBlockedBoard :: BlockedBoard -> BlockedBoard
expandBlockedBoard = expandBoard (Left False) 4 4

expandBoard :: a -> Int -> Int -> Array BIx a -> Array BIx a
expandBoard a w' h' b = emptyBoard a w' h' // map centre (assocs b)
  where
    centre ((i, j), e) = ((i + offset w w', j + offset h h'), e)
    offset x x' = (x' - x) `quot` 2
    (_, (w, h)) = bounds b

boardBounds :: Int -> Int -> (BIx, BIx)
boardBounds w h = ((1, 1), (w, h))

emptyBoard :: a -> Int -> Int -> Array BIx a
emptyBoard a w h = listArray (boardBounds w h) (replicate (w * h) a)

boardRows :: Array BIx a -> [[a]]
boardRows b = [[b ! (i,j) | i <- [i1..i2]] | j <- [j1..j2]]
  where ((i1,j1),(i2,j2)) = bounds b

-- board indices in row-major order
rowIndex :: Enum i => ((i,i),(i,i)) -> [(i,i)]
rowIndex ((i1,j1),(i2,j2)) = [(i,j) | j <- [j1..j2], i <- [i1..i2]]

inBoard :: Int -> Int -> Board -> Board -> Bool
-- fixme: could use functions on Ix class
inBoard x y piece board = x >= 0 && y >= 0 &&
                          w + x < bw && h + y < bh
  where (_, (w, h)) = bounds piece
        (_, (bw, bh)) = bounds board

-- drop a piece at a certain offset on the board
place :: Int -> Int -> Board -> Board -> Board
place x y piece = (// map move (filter (isJust . snd) (assocs piece)))
  where
    move ((i, j), e) = ((i + x, j + y), e)

intersection :: Board -> Int -> Int -> Board -> BlockedBoard
intersection board x y piece = array (bounds piece) es
  where
    es = [(ix, isect (get (move ix)) e) | (ix, e) <- assocs piece]
    isect (Just (Just _)) (Just _) = Left True
    isect (Just Nothing) (Just p) = Right p
    isect Nothing (Just _) = Left True
    isect _ Nothing = Left False
    move (i, j) = (i + x, j + y)
    get ix | inRange (bounds board) ix = Just (board ! ix)
           | otherwise = Nothing

isBlocked :: BlockedBoard -> Bool
isBlocked = any (== Left True)

rotate :: Bool -> Board -> Board
rotate dir b = ixmap ((1, 1), (h, w)) rot b
  where
    rot = if dir then ccw else cw
    cw (j, i) = (i, h - j + 1)
    ccw (j, i) = (w - i + 1, j)
    (_, (w, h)) = bounds b

rotateN :: Int -> Board -> Board
rotateN n = head . drop (abs n) . iterate (rotate False)

-- find columns or rows which are filled then clear them
eliminate :: Board -> (Int, Int, Board)
eliminate b = (length cols, length rows, b')
  where
    ((x1, y1), (x2, y2)) = bounds b
    find = filter (all isJust . map (b !))
    cols = find [[(i, j) | j <- [y1..y2]] | i <- [x1..x2]]
    rows = find [[(i, j) | i <- [x1..x2]] | j <- [y1..y2]]
    b' = clear (concat $ cols ++ rows) b

clear :: [BIx] -> Board -> Board
clear ixs b = b // [(ix, Nothing) | ix <- ixs]

isEmpty :: Board -> Bool
isEmpty = all isNothing

sunk :: Board -> Board -> Bool
-- If there is no way to place your piece, by rotating and translating
-- it, then you are sunk.
sunk board piece = all (sunk' board) (rotations piece)
  where
    rotations = nub . take 4 . iterate (rotate False)

sunk' :: Board -> Board -> Bool
sunk' b p = all (blockedAt b p) (placements b p)

placements :: Board -> Board -> [BIx]
placements board piece = [(i-1, j-1) | i <- [x1..x2-w+iw], j <- [y1..y2-h+ih]]
  where
    ((x1, y1), (x2, y2)) = bounds board
    ((iw, ih), (w, h)) = bounds piece

blockedAt :: Board -> Board -> BIx -> Bool
blockedAt board piece (x, y) = any (isJust . (board !)) filled
  where filled = [(i + x, j + y) | ((i, j), Just _) <- assocs piece]

instance Random Piece where
  randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                      (x, g') -> (toEnum x, g')
  random g = randomR (minBound, maxBound) g
