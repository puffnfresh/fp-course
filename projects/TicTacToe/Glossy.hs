module Glossy where

import Main hiding (main)
import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

crossPicture :: Picture
crossPicture =
  Line [(-30, -30), (30, 30)]
  <> Line [(-30, 30), (30, -30)]

circlePicture :: Picture
circlePicture =
  Circle 30

basePicture :: Picture
basePicture =
  Line [(-100, -33), (100, -33)]
  <> Line [(-100, 33), (100, 33)]
  <> Line [(-33, -100), (-33, 100)]
  <> Line [(33, -100), (33, 100)]

positionTranslation :: Position -> (Float, Float)
positionTranslation P1 =
  (-66, 66)
positionTranslation P2 =
  (0, 66)
positionTranslation P3 =
  (66, 66)
positionTranslation P4 =
  (-66, 0)
positionTranslation P5 =
  (0, 0)
positionTranslation P6 =
  (66, 0)
positionTranslation P7 =
  (-66, -66)
positionTranslation P8 =
  (0, -66)
positionTranslation P9 =
  (66, -66)

playerPicture :: Position -> Player -> Picture
playerPicture p pl =
  uncurry translate (positionTranslation p) pl'
  where
    pl' =
      case pl of
        O -> circlePicture
        X -> crossPicture

mainPicture :: CheckedMove MainMoves -> Picture
mainPicture (UnfinishedBoard m) =
  basePicture <> foldMap (uncurry playerPicture) (toPlayers m)
mainPicture (FinishedBoard (WonBoard m w)) =
  basePicture <> foldMap (uncurry highlightPlayer) (toPlayers m)
  where
    isWinner pl =
      case w of
        Draw ->
          True
        Winner pl' ->
          pl == pl'
    highlightPlayer p' pl =
      (if isWinner pl then Color (makeColorI 255 0 0 255) else id) (playerPicture p' pl)
mainPicture (BadMove _) =
  basePicture

data Column
  = L | M | R

data Row
  = T | C | B

handleEvent :: Event -> CheckedMove MainMoves -> CheckedMove MainMoves
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (UnfinishedBoard (MainMoves m)) =
  ignoreBadMove (check (MainMoves (pos row column : m)))
  where
    ignoreBadMove (BadMove _) =
      UnfinishedBoard (MainMoves m)
    ignoreBadMove a =
      a
    pos T L =
      P1
    pos T M =
      P2
    pos T R =
      P3
    pos C L =
      P4
    pos C M =
      P5
    pos C R =
      P6
    pos B L =
      P7
    pos B M =
      P8
    pos B R =
      P9
    row =
      if y > 33
      then T
      else if y < -33
      then B
      else C
    column =
      if x > 33
      then R
      else if x < -33
      then L
      else M
handleEvent _ m =
  m

main :: IO ()
main =
  play (InWindow "TicTacToe" (200, 200) (10, 10)) white 1 (UnfinishedBoard (MainMoves [])) mainPicture handleEvent (flip const)
