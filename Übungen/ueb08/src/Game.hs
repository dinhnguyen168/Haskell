{-# LANGUAGE FunctionalDependencies #-}
module Game where


-- A Game: a board with actions  
class Game b a | b-> a where
  terminal :: b -> Bool
  utility  :: b -> Float
  actions  :: b -> [a]
  result   :: b -> a -> b

-- Diese Implementierung funktioniert, sofern die Auswertungsfunktion nur betraglich Werte kleiner als 1000 annimmt.
minimax :: Game b a => b -> ([a], Float)
minimax s = maxi True [] s
  where
    -- Ich habe den Algorithmus auf Basis des Javacodes aus der Praktische Informatik 2-Vorlesung gebaut, die ich im 2. Semester gehoert habe.

    -- Nimmt den State und gibt eine Bewertung zurueck
    -- maxi :: Game b a => Bool -> [a] -> b -> ([a], Float)
    maxi firstAction actionList state
      | terminal state = (actionList, utility state)
      | otherwise      = foldr f ([], -1000) (actions state)
      where 
        -- f :: a -> ([a], Float) -> ([a], Float)
        f action e = if firstAction && snd e == snd nextMove then (fst nextMove ++ fst e, snd e) else (if snd nextMove > snd e then nextMove else e) where
          nextMove = mini (if firstAction then [action] else actionList) (result state action)

    -- mini :: Game b a => [a] -> b -> ([a], Float)
    mini actionList state
      | terminal state = (actionList, utility state)
      | otherwise      = foldr f ([], 1000) (actions state)
      where 
        -- f :: a -> ([a], Float) -> ([a], Float)
        f action e = if snd nextMove < snd e then nextMove else e where
          nextMove = maxi False (actionList) (result state action)
