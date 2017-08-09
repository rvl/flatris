{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module BoardDiagram ( boardDiagram, floatDiagram ) where

import Diagrams.Prelude
import Diagrams.TwoD.Combinators (strutR2)
import Data.Array (Array)

import Board
import Tango

boardSquare :: _ => Maybe Piece -> QDiagram b V2 n Any
boardSquare = maybe backgroundSquare pieceSquare

floatSquare :: _ => Either Bool Piece -> QDiagram b V2 n Any
floatSquare = either bad pieceSquare
  where
    bad True = redSquare
    bad False = strutR2 (pure 1)

backgroundSquare :: _ => QDiagram b V2 n Any
backgroundSquare = rect 1.0 1.0 # fc tangoAluminium1 # lc tangoAluminium3

redSquare :: _ => QDiagram b V2 n Any
redSquare = rect 1.0 1.0 # fc tangoScarletRed3 # lc tangoScarletRed2

pieceSquare :: _ => Piece -> QDiagram b V2 n Any
pieceSquare p = roundedRect 1.0 1.0 0.2 # lw medium # style p
  where
    style :: _ => Piece -> QDiagram b V2 n Any -> QDiagram b V2 n Any
    style OPiece = fc tangoButter1     # lc tangoButter3
    style IPiece = fc tangoChameleon1  # lc tangoChameleon3
    style LPiece = fc tangoOrange1     # lc tangoOrange3
    style JPiece = fc tangoSkyBlue1    # lc tangoSkyBlue3
    style TPiece = fc tangoPlum1       # lc tangoPlum3
    style SPiece = fc tangoChocolate1  # lc tangoChocolate3
    style ZPiece = fc tangoScarletRed1 # lc tangoScarletRed3

layout :: _ => (a -> QDiagram b V2 n Any) -> Array (Int, Int) a -> QDiagram b V2 n Any
layout f = vsep s . map (hsep s) . boardRows . fmap f
  where s = 0.1

boardDiagram :: _ => Board -> QDiagram b V2 n Any
boardDiagram = layout boardSquare

floatDiagram :: _ => BlockedBoard -> QDiagram b V2 n Any
floatDiagram = layout floatSquare
