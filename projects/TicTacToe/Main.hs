module Main where

import Control.Exception
import Control.Applicative

data Position
  = P1 | P2 | P3
  | P4 | P5 | P6
  | P7 | P8 | P9
  deriving (Eq, Ord, Show, Read)

data Board a
  = Board a a a
          a a a
          a a a
  deriving (Eq, Ord, Show)

data UnfinishedBoard
  = UnfinishedBoard (Board (Maybe Player))
  deriving (Eq, Ord, Show)

data Player
  = X | O
  deriving (Eq, Ord, Show)

data Winner
  = Winner Player | Draw
  deriving (Eq, Ord, Show)

data FinishedBoard
  = FinishedBoard (Board (Maybe Player)) Winner
  deriving (Eq, Ord, Show)

boardToList :: Board a -> [a]
boardToList (Board a b c d e f g h i) =
  [a, b, c, d, e, f, g, h, i]

boardPosition :: Position -> Board a -> a
boardPosition P1 (Board a _ _ _ _ _ _ _ _) =
  a
boardPosition P2 (Board _ a _ _ _ _ _ _ _) =
  a
boardPosition P3 (Board _ _ a _ _ _ _ _ _) =
  a
boardPosition P4 (Board _ _ _ a _ _ _ _ _) =
  a
boardPosition P5 (Board _ _ _ _ a _ _ _ _) =
  a
boardPosition P6 (Board _ _ _ _ _ a _ _ _) =
  a
boardPosition P7 (Board _ _ _ _ _ _ a _ _) =
  a
boardPosition P8 (Board _ _ _ _ _ _ _ a _) =
  a
boardPosition P9 (Board _ _ _ _ _ _ _ _ a) =
  a

setBoardPosition :: Position -> a -> Board a -> Board a
setBoardPosition P1 a (Board _ b c d e f g h i) =
  Board a b c d e f g h i
setBoardPosition P2 b (Board a _ c d e f g h i) =
  Board a b c d e f g h i
setBoardPosition P3 c (Board a b _ d e f g h i) =
  Board a b c d e f g h i
setBoardPosition P4 d (Board a b c _ e f g h i) =
  Board a b c d e f g h i
setBoardPosition P5 e (Board a b c d _ f g h i) =
  Board a b c d e f g h i
setBoardPosition P6 f (Board a b c d e _ g h i) =
  Board a b c d e f g h i
setBoardPosition P7 g (Board a b c d e f _ h i) =
  Board a b c d e f g h i
setBoardPosition P8 h (Board a b c d e f g _ i) =
  Board a b c d e f g h i
setBoardPosition P9 i (Board a b c d e f g h _) =
  Board a b c d e f g h i

data MovedBoard
  = FinishingMoveBoard FinishedBoard
  | MoveBoard UnfinishedBoard
  deriving (Eq, Ord, Show)

data MoveError
  = PositionNonEmpty Player
  deriving (Eq, Ord, Show)

whoseTurn :: Board (Maybe Player) -> Player
whoseTurn b =
  if countPlayer O == countPlayer X then X else O
  where
    countPlayer p =
      length $ filter (== Just p) (boardToList b)

threeq :: Eq a => a -> a -> a -> Bool
threeq a b c =
  a == b && b == c

boardWinner :: Board (Maybe Player) -> Maybe Winner
boardWinner b =
  w P1 P2 P3
  <|> w P1 P4 P7
  <|> w P3 P6 P9
  <|> w P7 P8 P9
  <|> w P1 P5 P9
  <|> w P7 P5 P3
  <|> if null $ filter (== Nothing) (boardToList b)
      then Just Draw
      else Nothing
  where
    w x y z = do
      x' <- boardPosition x b
      y' <- boardPosition y b
      z' <- boardPosition z b
      if threeq x' y' z'
      then Just $ Winner x'
      else Nothing

move :: Position -> UnfinishedBoard -> Either MoveError MovedBoard
move p (UnfinishedBoard b) =
  maybe (Right y) (Left . PositionNonEmpty) $ boardPosition p b
  where
    x =
      setBoardPosition p (Just $ whoseTurn b) b
    y =
      maybe (MoveBoard $ UnfinishedBoard x) (FinishingMoveBoard . FinishedBoard x)
        $ boardWinner x

whoWon :: FinishedBoard -> Winner
whoWon (FinishedBoard _ w) =
  w

class PlayerAt b where
  playerAt :: Position -> b -> Maybe Player

instance PlayerAt UnfinishedBoard where
  playerAt p (UnfinishedBoard b) =
    boardPosition p b

instance PlayerAt FinishedBoard where
  playerAt p (FinishedBoard b _) =
    boardPosition p b

startingBoard :: UnfinishedBoard
startingBoard =
  UnfinishedBoard $ Board Nothing Nothing Nothing
                          Nothing Nothing Nothing
                          Nothing Nothing Nothing

printBoard :: UnfinishedBoard -> IO ()
printBoard (UnfinishedBoard b) = do
  putStrLn $ take 3 x
  putStrLn $ take 3 $ drop 3 x
  putStrLn $ take 3 $ drop 6 x
  where
    x =
      f <$> boardToList b
    f (Just X) =
      'X'
    f (Just O) =
      'O'
    f Nothing =
      '.'

run :: UnfinishedBoard -> IO ()
run b = do
  printBoard b
  putStr "> "
  p <- try readLn :: IO (Either IOException Position)
  either (\_ -> run b) (\p' -> either print f $ move p' b) p
  where
    f (MoveBoard b') =
      run b'
    f (FinishingMoveBoard b) =
      print $ whoWon b

main :: IO ()
main =
  run startingBoard
