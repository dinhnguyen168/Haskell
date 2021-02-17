module Board(
    Player(..), otherP
  , Position
  , Board
  , rows
  , columns
  , moves
  , move
  , full
  , wins
  , emptyBoard
  , fromString
  , render
  ) where

import qualified Data.Map as Map
import Data.Map(Map)

-- Implementierung des Spielbretts --

data Player = O | X deriving (Eq, Ord, Show)

otherP :: Player-> Player
otherP X = O
otherP O = X

-- Game board
type Position = (Int, Int)
data Board = Board { 
  row :: Int,
  column :: Int,
  positionToPlayer :: Map Position Player 
  }

instance Eq Board where
  b1 == b2 = rows b1 == rows b2 
    && columns b1 == columns b2 
    && positionToPlayer b1 == positionToPlayer b2 -- Es muessen alle Werte des Boards uebereinstimmen, damit zwei Boards gleich sind.

instance Show Board where
  show b = unlines $ filter (\r-> r /= concat (take (columns b) (repeat " "))) (lines $ render b) -- Filtern der leeren Zeilen, wie in der Aufgabenstellung erwaehnt.

rows :: Board -> Int
rows = row

columns :: Board -> Int
columns = column

-- Print the board.
render :: Board-> String
render b = if wins b X || wins b O then go ((rows b) - 1) b "_" else go ((rows b) - 1) b "*" where

  -- Render markiert Felder nur mit Sternen, falls nur noch ein Feld noetig ist, damit eine Felderkombination zum Winningfield wird. Wir haben die Funktion render anscheinend falsch verstanden, jedoch haben wir nicht genug Zeit, diese Implementierung anzupassen. (Allgemein sind in diesem Uebungsblatt mehrere Aspekte mehrdeutig dargestellt, wodurch nicht ganz klar wird, was damit gemeint ist.) (Wir haben erst vor ca. 30 Minuten eine Antwort von Tobias Brandt erhalten, dass unsere Interpretation von render falsch ist.)

  go :: Int-> Board-> String-> String
  go acc b filler
    | acc == 0  = reverse $ renderRow acc (columns b - 1) b filler
    | acc >= 0  = (reverse $ renderRow acc (columns b - 1) b filler) ++ "\n" ++ go (acc - 1) b filler
    | otherwise = []

  -- Erzeugen der einzelnen Reihen
  renderRow :: Int-> Int-> Board-> String-> String
  renderRow r maxC b filler = go maxC b where
    go :: Int-> Board-> String
    go acc b 
      | acc >= 0  = (renderPosition (acc, r) b filler) ++ go (acc - 1) b
      | otherwise = []
  
  -- Interpretation eines einzelnen Zeichens
  renderPosition :: Position-> Board-> String-> String
  renderPosition p b filler = case Map.lookup p (positionToPlayer b) of
    Just player-> case player of
      X -> "X"
      O -> "O"
    Nothing-> case wins (move b X p) X  of
      True  -> filler
      False -> case wins (move b O p) O of
        True  -> filler
        False -> " "

-- Checks if this player has won
wins :: Board-> Player-> Bool
wins b p = winInRow || winInColumn || winInDiagonal where
  -- Horizontales Ueberpruefen, ob mindestens 4 Steine nebeneinander liegen.
  winInRow :: Bool
  winInRow = forRow (rows b - 1) where
    forRow :: Int-> Bool
    forRow r 
      | r >= 0    = forCol (columns b - 1) 0 || forRow (r - 1)
      | otherwise = False where
        forCol :: Int-> Int-> Bool
        forCol col count 
          | count == 4 = True
          | col >= 0   = forCol (col - 1) $ evaluatePosition b p (col, r) count
          | otherwise  = False

  -- Vertikales Ueberpruefen, ob mindestens 4 Steine nebeneinander liegen.
  winInColumn :: Bool
  winInColumn = forCol (columns b - 1) where
    forCol :: Int-> Bool
    forCol col 
      | col >= 0  = forRow (rows b - 1) 0 || forCol (col - 1)
      | otherwise = False where
        forRow :: Int-> Int-> Bool
        forRow r count
          | count == 4 = True
          | r >= 0     = forRow (r - 1) $ evaluatePosition b p (col, r) count
          | otherwise  = False

  -- Ueberpruefen der Diagonalen, ob mindestens 4 Steine zusammen eine Siegeskombination ergeben.
  winInDiagonal :: Bool
  winInDiagonal = forCol (columns b - 1) || forRow (rows b - 1) where
    forCol :: Int-> Bool
    forCol col 
      | col >= 0  = checkDiagonal b (col, 0) p (-) (+) || checkDiagonal b (col, 0) p (+) (+) || forCol (col - 1)
      | otherwise = False

    forRow :: Int-> Bool
    forRow r
      | r >= 0    = checkDiagonal b (0, r) p (+) (+) || checkDiagonal b (0, r) p (+) (-) || forRow (r - 1)
      | otherwise = False

  -- Zählt, wie viele gleiche Steine in einer Reihe liegen.
  evaluatePosition :: Board-> Player-> Position-> Int-> Int
  evaluatePosition b p xy count = case Map.lookup xy $ positionToPlayer b of
    Just player -> if player == p then count + 1 else 0
    Nothing     -> 0

  -- Ueberpruefen der diagonalen Felder.
  checkDiagonal :: Board-> Position-> Player-> (Int-> Int-> Int)-> (Int-> Int-> Int)-> Bool
  checkDiagonal b xy p f1 f2 = go xy 0 where
    go :: Position-> Int-> Bool
    go (x,y) count
      | count == 4                                                        = True
      | x <= columns b - 1 && y <= rows b - 1 && x >= 0 && y >= 0 = go (x `f1` 1, y `f2` 1) $ evaluatePosition b p (x, y) count
      | otherwise                                                         = False

-- Make one move (does not check wether legal or not)
move :: Board-> Player-> Position-> Board
move b p xy = b{ positionToPlayer= Map.insert xy p (positionToPlayer b) }


-- All possibles moves by position
moves :: Board-> [Position]
moves b = reverse (filter (\p-> moveIsViable p b && (Map.notMember p $ positionToPlayer b)) $ allPositions b ) where
  -- Testet, ob der Stein in der Luft hängen würde. Falls ja, wird False zurueckgegeben, ansonsten True.
  moveIsViable :: Position-> Board-> Bool
  moveIsViable (x,y) b = if y == 0 && (Map.notMember (x,y) $ positionToPlayer b) then True else Map.member (x, (y - 1)) $ positionToPlayer b

-- The board is full when the topmost row is filled in all columns
full :: Board-> Bool
full b = null $ filter (\p -> Map.notMember (p, rows b) $ positionToPlayer b) [1..(columns b)]
-- full b = null (filter (\p-> Map.notMember p $ positionToPlayer b) $ allPositions b) 

-- The empty board
emptyBoard  :: Int-> Board
emptyBoard sz = Board sz (sz + 1) Map.empty

fromString :: String-> Board
fromString s = case not . null $ lines s of -- Falls der String nicht leer ist.
  True-> let
    parse :: [String]-> Board-> Int-> Board
    parse s b r = case s of
      (s:xs)-> parse xs (foldr (\(c, i) result-> if c == 'X' || c == 'O' then result { positionToPlayer= Map.insert (i, (r - 1)) (charToPlayer c) $ positionToPlayer result } else result ) b (zip s [0..])) (r - 1)
      []-> b

    charToPlayer :: Char-> Player
    charToPlayer c = if c == 'O' then O else X -- Umwandlung eines Characters in einen Player
        
    r = length $ lines s -- die Hoehe des Strings
    c = length . head $ lines s -- die Breite des Strings
    equalLength = foldr (\i result-> result && i == c) True $ map (\line-> length line) (lines s) -- Pruefen, ob die Laengen der Zeilen uebereinstimmen.
    in if equalLength then parse (lines s) (Board (c - 1) c Map.empty) r else emptyBoard r -- Falls die Laengen uebereinstimmen, so gebe das Board aus, ansonsten erstelle ein leeres Board mit der Groesse der Hoehe des Strings. In der Aufgabenstellung ist nicht defininiert, was in diesem Fall geschehen soll, weshalb wir diesen Weg gewaehlt haben.

  False-> emptyBoard 6 -- Falls der String leer ist, gebe ein Board mit Standardgroesse 7 (wenn man die Breite betrachtet, die um 1 groesser als die von emptyBoard uebergebene groesse ist).


-- Hilfsmethode, die alle möglichen Positionen des Boards zurückgibt
allPositions :: Board-> [Position]
allPositions b = go b $ columns b - 1 where
  go :: Board-> Int-> [Position]
  go b acc
    | acc >= 0  = zip (take (rows b) $ repeat acc) (take (rows b) [0..]) ++ go b (acc - 1)
    | otherwise = []
