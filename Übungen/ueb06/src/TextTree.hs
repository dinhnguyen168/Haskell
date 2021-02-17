module TextTree where

import Data.Char

data WEntry = WEntry {word :: String, occurrences :: [Int]} deriving (Show, Eq)
-- the entry of a word with the list of the lines of its occurrences

data TTree a = WNode {rightbranch :: TTree a, entry :: a, leftbranch :: TTree a} | Empty deriving (Show, Eq)
-- the binary tree of word entries; entries in right branches shall be smaller

-- | Funktion, die eine ganze Zahl als "Occurence" zu einem Eintrag hinzufuegt.
addOccurrence :: WEntry -> Int -> WEntry
addOccurrence wentry occs = WEntry (word wentry) (occurrences wentry ++ [occs])

-- | Einfuegen eines neuen Eintrags des Typs WEntry in den Baum.
-- In der Aufgabenstellung ist nicht definiert, was passieren soll, wenn ein Wort zweimal in einer Zeile vorkommt. (Also ob die Menge der occurences dann zum Beipsiel [2,2] lauten kann oder einfach nur [2], falls ein wort zwei Mal in Zeile zwei vorkommt.) Daher habe ich mich dafuer entschieden, diese mehrfach einzufuegen. (Im Beispiel entspraeche dies [2,2].)
insertWord :: String -> Int -> TTree WEntry -> TTree WEntry
insertWord str rowNum Empty = WNode Empty (WEntry str [rowNum]) Empty -- Falls Knoten noch nicht existiert, so erstelle diesen.
insertWord str rowNum wnode
    | word (entry wnode) == str = WNode (rightbranch wnode) (WEntry str (occurrences (entry wnode) ++ [rowNum])) (leftbranch wnode) -- Fuege an dieser Stelle ein, falls bereits existiert.
    | word (entry wnode) > str  = WNode (insertWord str rowNum (rightbranch wnode)) (entry wnode) (leftbranch wnode) -- Fuege in rechten Teilbaum ein.
    | word (entry wnode) < str  = WNode (rightbranch wnode) (entry wnode) (insertWord str rowNum (leftbranch wnode)) -- Fuege in linken Teilbaum ein.

-- | Funktion, die einen String in eine Liste von Tupeln mit einem Wort und dessen Zeilennummern umwandelt.
wordsWithLine :: String -> [(String, Int)]
wordsWithLine = (f 1) . (filter (\x -> isAlpha x || x == '\n' || x == ' ')) where -- im String sollen nur alphabetische Zeichen betrahtet werden, weshalb der Rest herausgefiltert werden muss. Allein Leerzeichen und \n werden fuer die weitere Aufloesung benoetigt.
    -- Funktion, die die anfaengliche Zeilennummer sowie einen String nimmt und daraus mithilfe der Funktion getNextWord das naechste Wort nimmt, das nicht "" entspricht, und daraus eine Liste von Tupeln von Wort und Zeilennummer bildet
    f :: Int -> String -> [(String, Int)]
    f _ []  = []
    f n str
        | fst y /= "" = (fst y, n) : listTail
        | otherwise   = listTail
        where
        listTail = f (if snd y then n+1 else n) (drop (length (fst y)+1) str) -- +1, da das Leerzeichen/der Zeilenumbruch entfernt werden muss.
        y = getNextWord str -- das naechste Wort
        
    -- | Nimm das naechste durch Whitespace oder '\n' getrennte Wort in Kleinbuchstaben. Ist das Wort durch Whitespace getrennt, ist der Bool-Wert False, bei einem '\n' wird ein True zurueckgegeben. Der zweite String entspricht dem restlichen String.
    getNextWord :: String -> (String, Bool)
    getNextWord [] = ([], False)
    getNextWord (y:ys)
        | y == ' '  = ([], False)
        | y == '\n' = ([], True)
        | otherwise = (toLower y : fst (getNextWord ys), snd (getNextWord ys))
        
-- | Fuege eine Liste von Tupeln von Strings und Ints in einen Baum ein.
-- Es wird daher die Liste von Tupeln sowie der Baum, in den die Elemente eingefuegt werden sollen, benoetigt.
wordListToTree :: [(String, Int)] -> TTree WEntry -> TTree WEntry
wordListToTree list tree = foldl (\e x -> insertWord (fst x) (snd x) e) tree list -- Weil in den Angabe von foldl und wordListToTree die Parameter list und tree vertauscht sind, kann man das, meines Wissens nach, nur umstaendlich reduzieren.

-- | Funktion, die einen String in einen Baum umwandelt.
textToTTree :: String -> TTree WEntry
textToTTree str = wordListToTree (wordsWithLine str) Empty

{----------------------------}    

text1 = "eins zwei\ndrei vier\nfuenf sechs"

raven = "The Raven\nBY EDGAR ALLAN POE\nOnce upon a midnight dreary, while I pondered, weak and weary,\nOver many a quaint and curious volume of forgotten lore—\nWhile I nodded, nearly napping, suddenly there came a tapping,\nAs of some one gently rapping, rapping at my chamber door.\n’Tis some visitor, I muttered, tapping at my chamber door—\nOnly this and nothing more. \n\nAh, distinctly I remember it was in the bleak December;\nAnd each separate dying ember wrought its ghost upon the floor.\nEagerly I wished the morrow;—vainly I had sought to borrow\nFrom my books surcease of sorrow—sorrow for the lost Lenore—\nFor the rare and radiant maiden whom the angels name Lenore—\nNameless here for evermore.\n\nAnd the silken, sad, uncertain rustling of each purple curtain\nThrilled me—filled me with fantastic terrors never felt before;\nSo that now, to still the beating of my heart, I stood repeating\n’Tis some visitor entreating entrance at my chamber door—\nSome late visitor entreating entrance at my chamber door;—\nThis it is and nothing more. \n\nPresently my soul grew stronger; hesitating then no longer,\nSir, said I, or Madam, truly your forgiveness I implore;\nBut the fact is I was napping, and so gently you came rapping,\nAnd so faintly you came tapping, tapping at my chamber door,\nThat I scarce was sure I heard you —here I opened wide the door;—\nDarkness there and nothing more.\n\nDeep into that darkness peering, long I stood there wondering, fearing,\nDoubting, dreaming dreams no mortal ever dared to dream before;\nBut the silence was unbroken, and the stillness gave no token,\nAnd the only word there spoken was the whispered word, Lenore? \nThis I whispered, and an echo murmured back the word, Lenore!\nMerely this and nothing more.\n\nBack into the chamber turning, all my soul within me burning,\nSoon again I heard a tapping somewhat louder than before.\nSurely, said I, surely that is something at my window lattice;\nLet me see, then, what thereat is, and this mystery explore—\nLet my heart be still a moment and this mystery explore;—\n’Tis the wind and nothing more! \n\nOpen here I flung the shutter, when, with many a flirt and flutter,\nIn there stepped a stately Raven of the saintly days of yore;\nNot the least obeisance made he; not a minute stopped or stayed he;\nBut, with mien of lord or lady, perched above my chamber door—\nPerched upon a bust of Pallas just above my chamber door—\nPerched, and sat, and nothing more.\n\nThen this ebony bird beguiling my sad fancy into smiling,\nBy the grave and stern decorum of the countenance it wore,\nThough thy crest be shorn and shaven, thou, I said, art sure no craven,\nGhastly grim and ancient Raven wandering from the Nightly shore—\nTell me what thy lordly name is on the Night’s Plutonian shore! \nQuoth the Raven Nevermore. \n\nMuch I marvelled this ungainly fowl to hear discourse so plainly,\nThough its answer little meaning—little relevancy bore;\nFor we cannot help agreeing that no living human being\nEver yet was blessed with seeing bird above his chamber door—\nBird or beast upon the sculptured bust above his chamber door,\nWith such name as Nevermore. \n\nBut the Raven, sitting lonely on the placid bust, spoke only\nThat one word, as if his soul in that one word he did outpour.\nNothing farther then he uttered—not a feather then he fluttered—\nTill I scarcely more than muttered Other friends have flown before—\nOn the morrow he will leave me, as my Hopes have flown before. \nThen the bird said Nevermore. \n\nStartled at the stillness broken by reply so aptly spoken,\nDoubtless, said I, what it utters is its only stock and store\nCaught from some unhappy master whom unmerciful Disaster\nFollowed fast and followed faster till his songs one burden bore—\nTill the dirges of his Hope that melancholy burden bore\nOf ‘Never—nevermore’. \n\nBut the Raven still beguiling all my fancy into smiling,\nStraight I wheeled a cushioned seat in front of bird, and bust and door;\nThen, upon the velvet sinking, I betook myself to linking\nFancy unto fancy, thinking what this ominous bird of yore—\nWhat this grim, ungainly, ghastly, gaunt, and ominous bird of yore\nMeant in croaking Nevermore. \n\nThis I sat engaged in guessing, but no syllable expressing\nTo the fowl whose fiery eyes now burned into my bosom’s core;\nThis and more I sat divining, with my head at ease reclining\nOn the cushion’s velvet lining that the lamp-light gloated o’er,\nBut whose velvet-violet lining with the lamp-light gloating o’er,\nShe shall press, ah, nevermore!\n\nThen, methought, the air grew denser, perfumed from an unseen censer\nSwung by Seraphim whose foot-falls tinkled on the tufted floor.\nWretch, I cried, thy God hath lent thee—by these angels he hath sent thee\nRespite—respite and nepenthe from thy memories of Lenore;\nQuaff, oh quaff this kind nepenthe and forget this lost Lenore! \nQuoth the Raven Nevermore. \n\nProphet! said I, thing of evil!—prophet still, if bird or devil!—\nWhether Tempter sent, or whether tempest tossed thee here ashore,\nDesolate yet all undaunted, on this desert land enchanted—\nOn this home by Horror haunted—tell me truly, I implore—\nIs there—is there balm in Gilead?—tell me—tell me, I implore! \nQuoth the Raven Nevermore. \n\nProphet! said I, thing of evil!—prophet still, if bird or devil!\nBy that Heaven that bends above us—by that God we both adore—\nTell this soul with sorrow laden if, within the distant Aidenn,\nIt shall clasp a sainted maiden whom the angels name Lenore—\nClasp a rare and radiant maiden whom the angels name Lenore. \nQuoth the Raven Nevermore. \n\nBe that word our sign of parting, bird or fiend! I shrieked, upstarting—\nGet thee back into the tempest and the Night’s Plutonian shore!\nLeave no black plume as a token of that lie thy soul hath spoken!\nLeave my loneliness unbroken!—quit the bust above my door!\nTake thy beak from out my heart, and take thy form from off my door! \nQuoth the Raven Nevermore. \n\nAnd the Raven, never flitting, still is sitting, still is sitting\nOn the pallid bust of Pallas just above my chamber door;\nAnd his eyes have all the seeming of a demon’s that is dreaming,\nAnd the lamp-light o’er him streaming throws his shadow on the floor;\nAnd my soul from out that shadow that lies floating on the floor\nShall be lifted—nevermore!\n"
