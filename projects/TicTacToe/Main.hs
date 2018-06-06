module Main where

data Position
  = P1 | P2 | P3
  | P4 | P5 | P6
  | P7 | P8 | P9
  deriving Show

data Player
  = X
  | O
  deriving Show

type Cell
  = Maybe Player

data Row a
  = Row a a a
  deriving Show

type Board a
  = Row (Row a)

example :: Board Cell
example =
  Row
    (Row Nothing (Just X) Nothing )
    (Row Nothing Nothing  Nothing )
    (Row Nothing Nothing  (Just O))

updateBoard :: Position -> a -> Board a -> Board a
updateBoard P1 a (Row (Row _ b c) d e) =
  Row (Row a b c) d e
updateBoard P2 b (Row (Row a _ c) d e) =
  Row (Row a b c) d e
updateBoard P3 c (Row (Row a b _) d e) =
  Row (Row a b c) d e
updateBoard P4 b (Row a (Row _ c d) e) =
  Row a (Row b c d) e
updateBoard P5 c (Row a (Row b _ d) e) =
  Row a (Row b c d) e
updateBoard P6 d (Row a (Row b c _) e) =
  Row a (Row b c d) e
updateBoard P7 c (Row a b (Row _ d e)) =
  Row a b (Row c d e)
updateBoard P8 d (Row a b (Row c _ e)) =
  Row a b (Row c d e)
updateBoard P9 e (Row a b (Row c d _)) =
  Row a b (Row c d e)

indexBoard :: Position -> Board a -> a
indexBoard P1 (Row (Row a _ _) _ _) = a
indexBoard P2 (Row (Row _ a _) _ _) = a
indexBoard P3 (Row (Row _ _ a) _ _) = a
indexBoard P4 (Row _ (Row a _ _) _) = a
indexBoard P5 (Row _ (Row _ a _) _) = a
indexBoard P6 (Row _ (Row _ _ a) _) = a
indexBoard P7 (Row _ _ (Row a _ _)) = a
indexBoard P8 (Row _ _ (Row _ a _)) = a
indexBoard P9 (Row _ _ (Row _ _ a)) = a

turn :: Board Cell -> Player
turn _ =
  X

data InvalidMove
  = PositionTaken Position
  deriving Show

data MoveResult a
  = InvalidMove InvalidMove
  | ValidMove a
  deriving Show

move :: Position -> Board Cell -> MoveResult (Board Cell)
move p b =
  case indexBoard p b of
    Just c ->
      InvalidMove (PositionTaken p)
    Nothing ->
      ValidMove (updateBoard p Nothing b)

main :: IO ()
main =
  print example
