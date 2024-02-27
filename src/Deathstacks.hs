module Deathstacks where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char
import Board
import Data.List ( nubBy, elemIndex, elemIndices, find )
import Data.Maybe (isNothing)
import Data.Char ( chr )



-- #############################################################################
-- ########################### GIVEN IMPLEMENTATION ############################
-- #############################################################################

data Move = Move {start :: Pos, target :: Pos, steps :: Int}

instance Show Move where
  show (Move (Pos startC startR) (Pos tarC tarR) tr) = [startC] ++ show startR ++ "-" ++ show tr ++ "-" ++ [tarC] ++ show tarR

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) (Move (Pos sc2 sr2) (Pos tc2 tr2) r2) =
      sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2 && r1 == r2 


-- #############################################################################
-- #################### playerWon :: Board -> Maybe Player #####################
-- #################### - 4 Functional Points              #####################
-- #################### - 1 Coverage Point                 #####################
-- #############################################################################

-- Ein spieler hat verloren, wenn er keine Züge mehr machen kann
-- man kann keine Züge mehr machen, wenn alle türme vom gegnerischen spieler gekapert sind
-- man hat gewonnen , wenn auf jedem turm dein stein als oberstes liegt
-- man hat verloren, wenn jeder wenn der gegner seinen stein als oberstes hat überall

playerWon :: Board -> Maybe Player
playerWon b 
  | checkBlue b = Just Blue
  | checkRed b = Just Red
  | otherwise = Nothing

-- wir brauchen ein prädikat, dass schaut ob alle nicht emtpy stacks ganz oben blau ist, also des erste element im stack blau ist
-- für alle! entweder empty oder er Blau ist erstes element im stack
checkBlue :: Board -> Bool
checkBlue [] = True
checkBlue (x:xs) =   all checkBlueCell x && checkBlue xs

-- checks if for a given cell the cinditions are fulfilled when checking for Blue
checkBlueCell :: Cell -> Bool
checkBlueCell Empty = True
checkBlueCell (Stack players) = head players == Blue

checkRed :: Board -> Bool
checkRed [] = True
checkRed (x:xs) = all checkRedCell x && checkRed xs

-- checks if for a given cell the cinditions are fulfilled when checking for Red
checkRedCell :: Cell -> Bool
checkRedCell Empty = True
checkRedCell (Stack players) = head players == Red

-- #############################################################################
-- ################### possibleMoves :: Pos -> Cell -> [Move] ##################
-- ################### - 4 Functional Points                  ##################
-- ################### - 1 Coverage Point                     ##################
-- #############################################################################

-- eine zustandsveränderung ist es wenn nach einem Move Board_previous != Board_current
---- wir brauchen, alle züge aus jedem path von der maximalen anzahl an steinen
-- rausfiltern müssen wir, wenn es im path einen "zug" gibt der auf dem exakt selben Feld landet wie das anfangsfeld, dann würde es nämlich keine Zustandsveränderung im FEN string geben


-- wir brauchen eine Funktion, die alle Richtungen clockwise ausführt und die PathToMoves funtione aufruft
-- path sollte immer den Path für die maximale Anzahl an Steinen ausführen.
possibleMoves :: Pos -> Cell -> [Move]
possibleMoves p (Stack players) = removeDuplicates $ filter sameStateStepsPredicate ( pathToMoves (path p North (length players) ) 1 ++ pathToMoves (path p NorthEast (length players) ) 1
                                   ++ pathToMoves (path p East (length players) ) 1 ++ pathToMoves (path p SouthEast (length players) ) 1 ++ pathToMoves (path p South (length players) ) 1
                                   ++ pathToMoves (path p SouthWest (length players) ) 1 ++ pathToMoves (path p West (length players) ) 1 ++ pathToMoves (path p NorthWest (length players) ) 1)

pathToMoves :: [Pos] -> Int -> [Move]
pathToMoves [x] _ = []
pathToMoves (x:y:xs) steps =  Move x y steps : pathToMoves (x:xs) (steps + 1)
                                   
-- diese Funktion filtert alle Züge heraus gibt True zurück, sollte ein Zug auf dem gleichen Feld enden, wo es angefangen hat. Sonst False
sameStateStepsPredicate :: Move -> Bool
sameStateStepsPredicate m = start m /= target m
                                   
-- jetzt brauchen wir noch eine Funktion, die uns die Duplikate einer Liste herausfiltert
-- hier von ChatGPT inpspirieren lassen mit der nub function
-- das war der Output vin Prompt
--removeDuplicates :: [Move] -> [Move]
--removeDuplicates = nubBy (\m1 m2 -> start m1 == start m2 && target m1 == target m2 && steps m1 == steps m2)
removeDuplicates :: [Move] -> [Move]
removeDuplicates = nubBy (==)

-- #############################################################################
-- ################### isValidMove :: Board -> Move -> Bool ####################
-- ################### - 5 Functional Points                ####################
-- ################### - 1 Coverage Point                   ####################
-- #############################################################################

--isValidMove :: Board -> Move -> Bool
--isValidMove b m = tooTallRule (together b) (start m) && sameStateStepsPredicate m

isValidMove :: Board -> Move -> Bool
isValidMove b m = tooTallRule (together b) m (filterBoardTooTallStack b)  && sameStateStepsPredicate m

-- tooTallRule implemented vielleicht
-- true : allet jut
-- false : der move ist nicht, machbar
--tooTallRule :: Maybe Pos -> Pos -> Bool
--tooTallRule Nothing _ = True
--tooTallRule (Just a) b = a == b 

-- hier brauchen wir noch die Cell und wir müssen checken, ob steps - lenght stack <= 4 ist
tooTallRule :: Maybe Pos -> Move -> Maybe Cell -> Bool
tooTallRule Nothing _ _ = True
tooTallRule (Just a) m (Just (Stack p))  = (a == start m) && length p - steps m <= 4

-- brings two functions together
together :: Board -> Maybe Pos
together b = getPosition ( checkTooTall b) 6

--gets the position of a stack thats too tall if exist, else returns nothing
getPosition :: [[Bool]] -> Int ->  Maybe Pos
getPosition _ 0 = Nothing
getPosition (x:xs) row = if myIfClauseGetPosition (checkRow x row)   then getPosition xs (row-1) else toPos (checkRow x row)

--isNothing (fst (checkRow x row)) && elem (snd (checkRow x row)) [0..6]

-- took that implenebtation from chatGPT bit change the function name
myIfClauseGetPosition :: (Maybe Int, Int) -> Bool
myIfClauseGetPosition (Nothing, x) = x >= 0 && x <= 6
myIfClauseGetPosition _ = False


-- maybe gives back a tuple of coordibated in order to make a position out of it
checkRow :: [Bool] -> Int -> (Maybe Int, Int)
checkRow x row = (elemIndex True x, row)

-- takes coordinates and contructs a position out of it
toPos :: (Maybe Int, Int) -> Maybe Pos
toPos (Nothing, y) = Nothing
toPos (Just x , y) = Just (Pos (chr(x + 97)) y)

-- eine Funktion, die guckt ob dir Too Tall regel eingehalten wird
-- eine Stack players mit player als head and länge größer vier
checkTooTall :: Board -> [[Bool]]
checkTooTall  = map (map checkCell)

-- took this implementation from ChatGPT after teling him exactly what I need
checkCell :: Cell -> Bool
checkCell Empty = False  -- Empty cell
checkCell (Stack players) = length players > 4

--from chatGPt
filterBoardTooTallStack :: Board -> Maybe Cell
filterBoardTooTallStack board = find checkCell (concat board)

-- #############################################################################
-- ############# IMPLEMENT listMoves :: Board -> Player -> [Move] ##############
-- ############# - 2 Functional Points                            ##############
-- ############# - 1 Coverage Point                               ##############
-- #############################################################################


listMoves :: Board -> Player -> [Move]
listMoves b p = filter (isValidMove b) $ removeDuplicates $ moveForEveryPosition (getPositions b p) (getCells b p)

-- executes possibleMove for Every Pos and Cell
-- Tests: sind [Pos] und [Cell] auch wirklich in der gleichen Reihenfolge, sonst funktioniert die Funktion nicht
moveForEveryPosition :: [Pos] -> [Cell] -> [Move]
moveForEveryPosition [] [] = []
moveForEveryPosition (x:xs) (y:ys) = possibleMoves x y ++ moveForEveryPosition xs ys


-- wir brauchen eine Funktion, die uns alle cells in einem Board herausholt , wo der player oben ist
-- hier habe ich es mit von ChatGPt vernessern lassen.
getCells :: Board -> Player -> [Cell]
getCells b p = filter (\cell -> cellPredicate cell p) (concat b)

-- Prädikat, um herauszufinden wessen stapel in der Zelle gehört
cellPredicate :: Cell -> Player -> Bool
cellPredicate Empty _ = False
cellPredicate (Stack players) Red = head players == Red
cellPredicate (Stack players) Blue = head players == Blue

-- wir brauchen eine Funktion, die uns alle positions aus einem board herausholt wo der player oben ist
getPositions :: Board -> Player -> [Pos]
getPositions b p = makePositions (toBoolBoard b p) 6

-- converts the Board to [[Bool]] to work further with it
toBoolBoard :: Board -> Player -> [[Bool]]
toBoolBoard b p = map (map (\ x -> cellPredicate x p)) b

-- eine funktion, die aus der Bool list positions macht
makePositions :: [[Bool]] -> Int -> [Pos]
makePositions _ 0 = []
makePositions (x:xs) row =  tuplesToPositions (getPosFromRow x row) ++ makePositions xs (row-1)

-- takes a list of integers tuples (Index coordinates) and converts it to Positions
tuplesToPositions :: [(Int, Int)] -> [Pos]
tuplesToPositions [] = []
tuplesToPositions ((x,y):xs) =  [Pos (chr(x + 97)) y] ++ tuplesToPositions xs


--hier auch mit chatGPT helfen lassen, gerade mit der Repeat functionality
getPosFromRow :: [Bool] -> Int -> [(Int, Int)]
getPosFromRow b row = zip (elemIndices True b) (repeat row)