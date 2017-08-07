{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Display ( boardDiagram, floatDiagram ) where

import Diagrams.Prelude
import Diagrams.TwoD.Combinators (strutR2)
import Data.Array (Array)

import Board

boardSquare :: _ => Maybe Piece -> QDiagram b V2 n Any
boardSquare = maybe backgroundSquare pieceSquare

floatSquare :: _ => Either Bool Piece -> QDiagram b V2 n Any
floatSquare = either bad pieceSquare
  where
    bad True = redSquare
    bad False = strutR2 (pure 1)

backgroundSquare :: _ => QDiagram b V2 n Any
backgroundSquare = rect 1.0 1.0 # fc whitesmoke # lc lightgrey

redSquare :: _ => QDiagram b V2 n Any
redSquare = rect 1.0 1.0 # fc red # lc black

pieceSquare :: _ => Piece -> QDiagram b V2 n Any
pieceSquare p = roundedRect 1.0 1.0 0.2 # lw medium # style p
  where
    style :: _ => Piece -> QDiagram b V2 n Any -> QDiagram b V2 n Any
    style OPiece = fc red
    style IPiece = fc green
    style LPiece = fc brown
    style JPiece = fc cyan
    style TPiece = fc grey
    style SPiece = fc purple
    style ZPiece = fc yellow

layout :: _ => (a -> QDiagram b V2 n Any) -> Array (Int, Int) a -> QDiagram b V2 n Any
layout f = vsep s . map (hsep s) . boardRows . fmap f
  where s = 0.1

boardDiagram :: _ => Board -> QDiagram b V2 n Any
boardDiagram = layout boardSquare

floatDiagram :: _ => BlockedBoard -> QDiagram b V2 n Any
floatDiagram = layout floatSquare
