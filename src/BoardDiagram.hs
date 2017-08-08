{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module BoardDiagram ( boardDiagram, floatDiagram ) where

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

tangoButter1, tangoButter2, tangoButter3,
  tangoChameleon1, tangoChameleon2, tangoChameleon3,
  tangoOrange1, tangoOrange2, tangoOrange3,
  tangoSkyBlue1, tangoSkyBlue2, tangoSkyBlue3,
  tangoPlum1, tangoPlum2, tangoPlum3,
  tangoChocolate1, tangoChocolate2, tangoChocolate3,
  tangoScarletRed1, tangoScarletRed2, tangoScarletRed3,
  tangoAluminium1, tangoAluminium2, tangoAluminium3,
  tangoAluminium4, tangoAluminium5, tangoAluminium6 :: (Ord a, Floating a) => Colour a
tangoButter1     = sRGB24 252 233  79
tangoButter2     = sRGB24  237 212   0
tangoButter3     = sRGB24  196 160   0
tangoChameleon1  = sRGB24  138 226  52
tangoChameleon2  = sRGB24  115 210  22
tangoChameleon3  = sRGB24   78 154   6
tangoOrange1     = sRGB24  252 175  62
tangoOrange2     = sRGB24  245 121   0
tangoOrange3     = sRGB24  206  92   0
tangoSkyBlue1    = sRGB24 114 159 207
tangoSkyBlue2    = sRGB24  52 101 164
tangoSkyBlue3    = sRGB24  32  74 135
tangoPlum1       = sRGB24  173 127 168
tangoPlum2       = sRGB24  117  80 123
tangoPlum3       = sRGB24   92  53 102
tangoChocolate1  = sRGB24  233 185 110
tangoChocolate2  = sRGB24  193 125  17
tangoChocolate3  = sRGB24  143  89   2
tangoScarletRed1 = sRGB24 239  41  41
tangoScarletRed2 = sRGB24 204   0   0
tangoScarletRed3 = sRGB24 164   0   0
tangoAluminium1  = sRGB24 238 238 236
tangoAluminium2  = sRGB24 211 215 207
tangoAluminium3  = sRGB24 186 189 182
tangoAluminium4  = sRGB24 136 138 133
tangoAluminium5  = sRGB24  85  87  83
tangoAluminium6  = sRGB24  46  52  54
