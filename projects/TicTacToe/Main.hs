module Main where

import Control.Applicative
import Control.Exception

data Move1
  = Move1 Position
  deriving (Ord, Eq, Show)

data Move2
  = Move2 Position Move1
  deriving (Ord, Eq, Show)

data Move3
  = Move3 Position Move2
  deriving (Ord, Eq, Show)

data Move4
  = Move4 Position Move3
  deriving (Ord, Eq, Show)

data Move5
  = Move5 Position Move4
  deriving (Ord, Eq, Show)

data Move6
  = Move6 Position Move5
  deriving (Ord, Eq, Show)

data Move7
  = Move7 Position Move6
  deriving (Ord, Eq, Show)

data Move8
  = Move8 Position Move7
  deriving (Ord, Eq, Show)

data Move9
  = Move9 Position Move8
  deriving (Ord, Eq, Show)

class MovePositions m where
  movePositions :: m -> [Position] -- TODO: Should be NonEmpty

instance MovePositions Move1 where
  movePositions (Move1 p) =
    [p]

instance MovePositions Move2 where
  movePositions (Move2 p m) =
    p : movePositions m

instance MovePositions Move3 where
  movePositions (Move3 p m) =
    p : movePositions m

instance MovePositions Move4 where
  movePositions (Move4 p m) =
    p : movePositions m

instance MovePositions Move5 where
  movePositions (Move5 p m) =
    p : movePositions m

instance MovePositions Move6 where
  movePositions (Move6 p m) =
    p : movePositions m

instance MovePositions Move7 where
  movePositions (Move7 p m) =
    p : movePositions m

instance MovePositions Move8 where
  movePositions (Move8 p m) =
    p : movePositions m

instance MovePositions Move9 where
  movePositions (Move9 p m) =
    p : movePositions m

data Position
  = P1 | P2 | P3
  | P4 | P5 | P6
  | P7 | P8 | P9
  deriving (Eq, Ord, Show, Read)

data Player
  = X | O
  deriving (Eq, Ord, Show)

data Winner
  = Winner Player | Draw
  deriving (Eq, Ord, Show)

data MoveError
  = PositionNonEmpty Player
  deriving (Eq, Ord, Show)

whoseTurn :: MovePositions m => m -> Player
whoseTurn m =
  if even (length (movePositions m))
  then X
  else O

threeq :: Eq a => a -> a -> a -> Bool
threeq a b c =
  a == b && b == c

data WonBoard m
  = WonBoard m Winner
  deriving (Eq, Ord, Show)

data CheckedMove m
  = FinishedBoard (WonBoard m)
  | UnfinishedBoard m
  | BadMove MoveError
  deriving (Eq, Ord, Show)

boardWinner :: MovePositions m => m -> Maybe Winner
boardWinner b =
  w P1 P2 P3
  <|> w P1 P4 P7
  <|> w P3 P6 P9
  <|> w P2 P5 P8
  <|> w P4 P5 P6
  <|> w P7 P8 P9
  <|> w P1 P5 P9
  <|> w P7 P5 P3
  <|> if length ps == 9
      then Just Draw
      else Nothing
  where
    ps =
      toPlayers b
    w x y z = do
      x' <- lookup x ps
      y' <- lookup y ps
      z' <- lookup z ps
      if threeq x' y' z'
      then Just $ Winner x'
      else Nothing

toPlayers :: MovePositions m => m -> [(Position, Player)]
toPlayers m =
  reverse (zip (reverse (movePositions m)) (cycle [X, O]))

check :: MovePositions m => m -> CheckedMove m
check m =
  maybe validMove (BadMove . PositionNonEmpty) (inTail isTaken (toPlayers m))
  where
    validMove =
      maybe (UnfinishedBoard m) (FinishedBoard . WonBoard m) (boardWinner m)
    isTaken (po, _) xs =
      lookup po xs

inTail :: (a -> [a] -> Maybe b) -> [a] -> Maybe b
inTail _ [] =
  Nothing
inTail f (x:xs) =
  f x xs <|> inTail f xs

isDraw :: WonBoard Move9 -> Bool
isDraw =
  (== Draw) . whoWon

whoWon :: WonBoard m -> Winner
whoWon (WonBoard _ w) =
  w

playerAt :: MovePositions m => Position -> m -> Maybe Player
playerAt p =
  lookup p . toPlayers

printBoard :: MovePositions m => m -> IO ()
printBoard m = do
  printRow P1 P2 P3
  printRow P4 P5 P6
  printRow P7 P8 P9
  where
    ps =
      toPlayers m
    printRow a b c =
      putStrLn [g a, g b, g c]
    g =
      f . flip lookup ps
    f (Just X) =
      'X'
    f (Just O) =
      'O'
    f Nothing =
      '.'

data MainMoves
  = MainMoves { mainMoves :: [Position] }
  deriving (Ord, Eq, Show)

instance MovePositions MainMoves where
  movePositions (MainMoves ps) =
    ps

run :: MainMoves -> IO ()
run m = do
  printBoard m
  putStr "> "
  p <- try readLn :: IO (Either IOException Position)
  either (\_ -> run m) (\p' -> f (check (MainMoves (p' : mainMoves m)))) p
  where
    f (FinishedBoard (WonBoard m' w)) = do
      printBoard m'
      print w
    f (UnfinishedBoard m') =
      run m'
    f (BadMove p) = do
      putStrLn ("Bad move: " ++ show p)
      run m

main :: IO ()
main =
  run (MainMoves [])
