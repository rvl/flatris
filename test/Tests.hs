{-# LANGUAGE DeriveGeneric, RecordWildCards #-}

module Main where

import           Test.Tasty (defaultMain, testGroup)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.HUnit (Assertion, (@?=), (@?), assertFailure)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck

import Data.Array
import Data.Maybe (isNothing)

import Board
import BoardUtil

main :: IO ()
main = defaultMain $ testGroup "Flatris Rules"
    [ boardTests
    , readShowTests
    , placementTests
    , sunkTests
    ]

readShowTests :: TestTree
readShowTests = testGroup "Read/Show"
                [ testProperty "showPiece . readPiece == id"
                  prop_show_read_piece
                , testProperty "readPiece . showPiece ~~ id"
                  prop_read_show_piece
                , testProperty "showBoard . readBoard == id"
                  (prop_show_read . unBoard')
                ]

boardTests :: TestTree
boardTests = testGroup "Board Operations"
             [ testProperty "rotate^4 == id ccw"
               (prop_rotate_symmetry False . unBoard')
             , testProperty "rotate^4 == id cw"
               (prop_rotate_symmetry True . unBoard')
             , testProperty "rotation preserves histogram of elements"
               (prop_rotate_preserves . unBoard')
             , testProperty "there are never full lines after elimination"
               (prop_eliminate_works . unBoard')
             , testCase "rotation" (boardTest rotateTest)
             , testCase "placement 1" (boardTest placeTest1)
             , testCase "placement 2" (boardTest placeTest2)
             ]

placementTests :: TestTree
placementTests = testGroup "Piece Placement"
                 [ blockedBoardTest intersectionTest0
                 , blockedBoardTest intersectionTest1
                 , blockedBoardTest intersectionTest2
                 , blockedBoardTest intersectionTest3
                 , blockedBoardTest intersectionTest4
                 , blockedBoardTest intersectionTest5
                 ]

instance Arbitrary Piece where
  arbitrary = arbitraryBoundedEnum

-- wrap the array newtype so we can give it instances
-- maybe would be better to do this in board.hs
newtype Board' = Board' { unBoard' :: Board } deriving (Eq)

instance Show Board' where
  show = showBoard . unBoard'

instance Arbitrary Board' where
  arbitrary = do
     w <- sized pure
     h <- sized pure
     es <- vectorOf (w * h) arbitrary
     return . Board' $ listArray ((1, 1), (w, h)) es

prop_show_read_piece :: Maybe Piece -> Bool
prop_show_read_piece p = readPiece (showPiece p) == p

prop_read_show_piece :: Char -> Bool
prop_read_show_piece c = c' == c || c' == '.'
  where c' = showPiece (readPiece c)

prop_show_read :: Board -> Bool
prop_show_read b = readBoard (showBoard b) == b

prop_rotate_symmetry :: Bool -> Board -> Bool
prop_rotate_symmetry dir b = rotate4 b == b
  where rotate4 = head . drop 4 . iterate (rotate dir)

prop_rotate_preserves :: Board -> Bool
prop_rotate_preserves b = histogram b == histogram (rotate True b)
  where
    histogram :: Board -> Array Piece Int
    histogram board = accumArray (+) 0 (minBound, maxBound) ps
      where ps = [(p, 1) | Just p <- elems board]

prop_eliminate_works :: Board -> Bool
--  after elimination, there are never full lines
prop_eliminate_works b = and vtest && and htest
  where
    (_, _, b') = eliminate b
    vtest = [any isNothing [b' ! (i, j) | i <- [x1..x2]] | j <- [y1..y2]]
    htest = [any isNothing [b' ! (i, j) | j <- [y1..y2]] | i <- [x1..x2]]
    ((x1, y1), (x2, y2)) = bounds b'

rotateTest = ([ "..OO..  .O..."
              , "....O.  ..O.."
              , ".OOOOO  ..O.O"
              , "O...O.  O.O.O"
              , "...O..  .OOO."
              , "        ..O.."
              ], rotate False)

placeTest1 =  ([ "OOOOO  OOOOO"
               , "O...O  O.T.O"
               , "O...O  OTTTO"
               , "O...O  O...O"
               , "OOOOO  OOOOO"
               ], place 1 1 tPiece)

placeTest2 =  ([ ".OO.  JOO."
               , "....  JJJ."
               ], place 0 0 jPiece)

type BoardTest = ([String], Board -> Board)

boardTest :: BoardTest -> Assertion
boardTest (bs, f) = Board' (f board) @?= Board' expected
  where
    (board, expected) = readTwoBoards bs


newtype BlockedBoard' = BlockedBoard' { unBlockedBoard' :: BlockedBoard } deriving (Eq)

instance Show BlockedBoard' where
  show = showBlockedBoard . unBlockedBoard'

type BlockedBoardTest = (String, [String], Board -> BlockedBoard)

-- blockedBoardTest :: BlockedBoardTest -> TestCase
blockedBoardTest (d, bs, f) = testCase d assertion
  where
    assertion = BlockedBoard' (f board) @?= BlockedBoard' blocked
    (board, blocked) = readBoardAndBlocked bs

intersectionTest0 = (
  "no intersection",
    [ "OOOOO  LLL"
    , "O...O  L.."
    , "O...O     "
    , "O...O     "
    , "OOOOO     "
    ], \b -> intersection b 1 1 lPiece)

intersectionTest1 = (
  "intersections at origin",
    [ "OOOOO  .X."
    , "O...O  XTT"
    , "O...O     "
    , "O...O     "
    , "OOOOO     "
    ], \b -> intersection b 0 0 tPiece)

intersectionTest2 = (
  "intersection within board",
    [ "OOOOO  IXIX"
    , "O...O      "
    , "O.O.O      "
    , "O...O      "
    , "OOOOO      "
    ], \b -> intersection b 1 2 iPiece)

intersectionTest3 =  (
  "intersection partially outside",
    [ "OOOOO  .X."
    , "O...O  TXX"
    , "O...O     "
    , "O...O     "
    , "OOOOO     "
    ], \b -> intersection b 3 1 tPiece)

intersectionTest4 =  (
  "intersection completely outside",
    [ "OOOOO  X.."
    , "O...O  XXX"
    , "O...O     "
    , "O...O     "
    , "OOOOO     "
    ], \b -> intersection b 6 1 jPiece)

intersectionTest5 =  (
  "intersection within shape board",
    [ ".OO..  J.."
    , ".....  JJJ"
    ], \b -> intersection b 0 0 jPiece)


sunkTests :: TestTree
sunkTests = testGroup "Sunk detection"
            [ testProperty
              "if there is a board containing a gap for the piece, then you are not sunk"
              prop_sunk1
            , testProperty
              "if there is a board containing a gap for a piece, then you are sunk for all other pieces."
              prop_sunk2
            ]

data SunkTestVars =
  SunkTestVars { sunkTestVarSize :: (Int, Int)
               , sunkTestVarPiece :: Piece
               , sunkTestVarRotate :: Int
               , sunkTestVarPos :: (Int, Int)
               } deriving (Show, Eq)

instance Arbitrary SunkTestVars where
  arbitrary = do
     w <- choose (4, 20)
     h <- choose (4, 20)
     p <- arbitrary
     r <- choose (0, 3)
     x <- choose (0, w - 1 - 3)
     y <- choose (0, h - 1 - 3)
     return $ SunkTestVars (w, h) p r (x, y)

data SunkTest = SunkTest { sunkTestBoard :: Board'
                         , sunkTestPiece :: Board'
                         , sunkTestPos :: (Int, Int)
                         } deriving (Show, Eq)

instance Arbitrary SunkTest where
  arbitrary = do
    SunkTestVars{..} <- arbitrary
    let (x, y) = sunkTestVarPos
        (w, h) = sunkTestVarSize
        b = invertBoard $ place x y p (emptyBoard Nothing w h)
        p = rotateN sunkTestVarRotate $ shapeBoard sunkTestVarPiece
    return $ SunkTest (Board' b) (Board' p) sunkTestVarPos

-- prop_sunk1 :: SunkTest -> Property
prop_sunk1 SunkTest{..} = not $ sunk (unBoard' sunkTestBoard) (unBoard' sunkTestPiece)

prop_sunk2 :: SunkTest -> Property
prop_sunk2 SunkTest{..} = inBoard x y pb b ==> all (sunk b) ps
  where
    (x, y) = sunkTestPos
    b = unBoard' sunkTestBoard
    pb = unBoard' sunkTestPiece
    ps = [rotateN r $ shapeBoard p | p <- otherThan tp, r <- [0..3]]
    Just tp = boardShape pb
