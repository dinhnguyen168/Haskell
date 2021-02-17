module Main where

import Board
import ComputerPlayer

import System.Random(randomRIO)
import System.Environment(getArgs)
import System.IO

data Winner = X | O | Draw deriving Show

-- Main function
-- Spielfeldgröße und Tiefe lassen sich durch Kommandozeielenargumente eingeben (wie im Tutorium von Tobias Brandt besprochen wurde)
-- Die uebergebenen Argumente muessen Zahlen sein. Ansonsten wird ein Parse-Fehler ausgegeben.
main :: IO ()
main = do
    args <- getArgs
    if length args == 2 then do
        let b = emptyBoard $ (read $ args !! 0) - 1
        let d = read $ args !! 1
        winner <- playRound b d
        let s = "Winner: " ++ show winner
        putStrLn s
        hFlush stdout
    else do
        winner <- playRound (emptyBoard 6) 4
        let s = "Winner: " ++ show winner
        putStrLn s
        hFlush stdout

-- Funktion, die den Ablauf einer Runde beinhaltet. Die Implementation hierzu laesst sich fast 1 zu 1 der Beschreibung aus der Aufgabenstellung entnehmen und bedarf somit keiner weiteren Erklaerung.
playRound :: Board-> Int-> IO Winner
playRound b depth = do
    putStr $ renderTable b
    if full b then do return Draw 
    else do
        b' <- executeMove (getInput b) b Board.O
        putStr $ renderTable b'
        if wins b' Board.O then do return Main.O 
        else do
            let comMoves = selectMove b' depth
            let rand = randomRIO (0, (length $ fst comMoves) - 1)
            r <- rand
            if full b then do return Draw 
            else do
                b'' <- executeMove (return $ (fst comMoves) !! r) b' Board.X
                if wins b'' Board.X then do return Main.X 
                else do 
                    playRound b'' depth
        
-- Ausfuehren eines Zuges eines Spielers auf einem Board.
executeMove :: IO Position-> Board-> Player-> IO Board
executeMove i b p = do
    x <- i
    return $ move b p x

-- Implementation von getInput analog zur Impelemtation der Methode "getInput" aus dem Tutorium von Tobias Brandt.
getInput :: Board-> IO Position
getInput b = do
  putStr "Your move? "
  hFlush stdout
  line <- getLine
  case reads line of
    [(x,_)] -> do
        let pos = filter (\(x', _)-> (x - 1) == x') $ moves b
        if not $ null pos then do pure $ head pos else do
            putStrLn "Invalid Input"
            getInput b
    _       -> do 
      putStrLn "Invalid Input"
      getInput b


-- Ausgeben der Tabelle mit Einfuegen des Rahmens. Als Grundlage hiervon wird die Funktion render benutzt, die das Spielfeld ohne Kuerzung und ohne Rand der leeren Zeilen, ausgibt.
renderTable :: Board-> String
renderTable b = let
    topBottom = (concat $ (take (columns b) (repeat "+-"))) ++ "+\n"
    bottom = take (columns b) . concat $ map (\i-> show i) [1..] in
        renderRows b 0 topBottom ++ topBottom ++ renderCells bottom ' '

-- Hilfsfunktion zur Ausgabe der einzelnen Zeilen.
renderRows :: Board-> Int-> String-> String
renderRows b acc topBottom
    | acc < (rows b) = let
        b' = render b in 
            topBottom ++ renderCells ((lines b') !! acc) '|' ++ renderRows b (acc + 1) topBottom
    | otherwise = []

-- Hilfsfunktionen zur Ausgabe der einzelnen Felder.
renderCells :: String-> Char-> String
renderCells b spacer = (foldl (\result c-> result ++ (spacer : []) ++ (c : [])) "" b) ++ (spacer : "\n")
