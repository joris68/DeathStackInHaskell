module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO ANY CHANGES TO package.yaml, e.g.:
--       import Data.Chars
import Data.Char
import Data.List
import Data.Maybe (isNothing)

-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = Red | Blue deriving Show
data Cell =  Stack [Player] | Empty deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
data Dir = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) Blue Blue = True
  (==) Red Red = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Stack xs) (Stack ys) = xs == ys
  (==) _ _ = False


-- #############################################################################
-- ################# IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Functional Points                  ###################
-- ################## - 1 Coverage Point                     ###################
-- #############################################################################

-- What I am checking for in validateFEN
-- 1. the lenght of the string should be exactly 59 every time, since no stones can be captured in this game
-- 2. when I split the string at the "/" into [String], every String should contain 5 commas every time!!!
-- 3. there should never be slashes the first or last character of the FEN String
-- 4. the string should contain exactly this amount of characters: 5 x '/' , 12 x 'b' , 12 x 'r'
-- 5. there should not be the any other characters in the string than these 4: ',' , '/' , 'b' , 'r'

validateFEN :: String -> Bool
validateFEN s = (length s == 59 || length s == 35)  && commas ( wordsWhen (\x->x=='/') s) && noSlashes s &&  enough s '/' 5  && (enough s 'b' 12 || enough s 'b' 0) && (enough s 'r' 12 || enough s 'r' 0) && invalidChars s

-- takes the sting splitted [String] by the slashes and checks if every string contains exactly 5 commas
commas :: [String] -> Bool
commas [] = True
commas (x:xs) = if enough x ',' 5 then commas xs else False

-- general predicate that checks characters are in a given string
-- but the general Idea was taken from ChatGPT und wurde angepast:
-- my Input to chatGPT: 
--enoughRs :: String -> Bool
--enoughRs s = length (filter(\x -> x == "r") s) == 5
-- Output:
--enoughRs :: String -> Bool
--enoughRs s = length (filter (\x -> x == 'r') s) == 5
-- so I generilzed it with more parameters to also check if there are enough other chars

enough :: String -> Char -> Int -> Bool
enough s c i = length (filter (\x -> x == c) s) == i

--von chatGPT verbessern lassen
-- Input:
--justChars :: String -> Bool
--justChars s = filter (\x -> /= 'r' || 'b' || ',' || '/' )
--Output:
--justChars :: String -> String
--justChars s = all (\x -> not (x `elem` "rb, /")) s
-- ich hab den namen der Funktion geändert zu invalidChars
invalidChars :: String -> Bool
invalidChars s = all (\x -> (x `elem` "rb,/")) s

-- slashes dürfen nicht am anfang sein und nicht am ende
noSlashes :: String -> Bool
noSlashes s = (take 1 s )!!0 /= '/' && (head $ reverse s) /= '/'

-- #############################################################################
-- ####################### buildBoard :: String -> Board #######################
-- ####################### - 2 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

-- first we are gonna split the string into a list of strings using wordsWhen
-- after that mapToStringCells taking over taking [String] -> [[String]]
-- thirdly we are gonna map [[String]] -> to [[cell]] with the cellmapper
buildBoard :: String -> Board
buildBoard s = cellMapper $ mapToStringCells $ wordsWhen (\x->x=='/') s

-- here I asked chatGPT "Ho di I traverse through a [[String]] in Haskell output should also be a list of lists"
-- So I took the map (map some predicate) idea from the asnwer
cellMapper :: [[String]] -> [[Cell]]
cellMapper = map (map toCell)

-- here I asked chatGPT how would I create Cell data types
toCell :: String -> Cell
toCell s
     | s == " " = Empty
     | otherwise = Stack $ toPlayers s


toPlayers:: String -> [Player]
toPlayers [] = []
toPlayers (x:xs) = if x == 'r' then Red : toPlayers xs
                    else
                    Blue : toPlayers xs

mapToStringCells :: [String] -> [[String]]
mapToStringCells [] = []
mapToStringCells (x:xs) = toStringRow x : mapToStringCells xs

toStringRow :: String -> [String]
toStringRow s = wordsWhen (\x->x==',') $ putCommas $ addSpace s


-- Mein Input War: putCommas :: String -> String
--putCommas [] = []
--putCommas (x:y:xs) = if x == ',' && y == ',' then x : ' ' : putCommas (y:xs)
            --      else x : putCommas (y:xs)
-- durch ChatGPT verbessern lassen!
putCommas :: String -> String
putCommas [] = []
putCommas [x] = [x]
putCommas (x:y:xs)
     | x == ',' && y == ',' = x : ' ' : putCommas (y:xs)
     | otherwise = x : putCommas (y:xs)

-- taken from StackOverFlow! -> the whole wordsWhen funtion!!
--https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell, accessed 04.12.2023
-- for breaking in the string at the "/" like the split function in other languages
-- using the function with the rigth! lambda function was my doing tho :)
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

-- mein Input  : addSpace :: String -> String
--addSpace s = if head s == ',' then " " : s
--if tail s == ',' then s : " "
-- durch ChatGPT verbessern lassen
addSpace :: String -> String
addSpace s = addSpaceEnd $ addSpaceBeginning s


addSpaceBeginning :: String -> String
addSpaceBeginning s
          | head s == ',' = ' ' : s
          | otherwise = s

addSpaceEnd :: String -> String
addSpaceEnd s
     | last s == ',' = s ++ " "
     | otherwise = s

-- #############################################################################
-- #################### path :: Pos -> Dir -> Int -> [Pos]  ####################
-- #################### - 4 Functional Points               ####################
-- #################### - 1 Coverage Point                  ####################
-- #############################################################################

-- implement Eq function for Dir Type for Unit test
instance Eq Dir where
  (==) North North = True
  (==) NorthEast NorthEast = True
  (==) East East = True
  (==) SouthEast SouthEast = True
  (==) South South = True
  (==) SouthWest SouthWest = True
  (==) West West = True
  (==) NorthWest NorthWest = True



performStep :: Pos -> Dir -> Pos
performStep start North = Pos (col start) (row start +1)
performStep start NorthEast = Pos (toEnum(ord(col start) +1)) (row start +1)
performStep start East = Pos (toEnum(ord(col start) +1)) (row start)
performStep start SouthEast = Pos (toEnum(ord(col start) +1)) (row start - 1)
performStep start South = Pos (col start) (row start - 1)
performStep start SouthWest = Pos (toEnum(ord(col start) - 1)) (row start - 1)
performStep start West = Pos (toEnum(ord(col start) - 1)) (row start)
performStep start NorthWest = Pos (toEnum(ord(col start) - 1)) (row start +1)

-- whole lot of mapper functions
northernMapper :: Dir -> Dir
northernMapper North = South
northernMapper NorthEast = SouthEast
northernMapper NorthWest = SouthWest

easternMapper :: Dir -> Dir
easternMapper East = West
easternMapper SouthEast = SouthWest
easternMapper NorthEast = NorthWest

southernMapper :: Dir -> Dir
southernMapper South = North
southernMapper SouthWest = NorthWest
southernMapper SouthEast = NorthEast

westernMapper :: Dir -> Dir
westernMapper West = East
westernMapper SouthWest = SouthEast
westernMapper NorthWest = NorthEast

northEasternMapper :: Dir -> Dir
northEasternMapper NorthEast = SouthWest
--northEasternMapper _ = error "wie kommt es zu einer anderen richtung?"

southEasternMapper :: Dir -> Dir
southEasternMapper SouthEast = NorthWest
--southEasternMapper _ = error "Wie kommt es zu einer anderen richtung?"

southWesternMapper :: Dir -> Dir
southWesternMapper SouthWest = NorthEast
--southWesternMapper _ = error "Wir kommt da ein anderer"

northWesternmapper :: Dir -> Dir
northWesternmapper NorthWest = SouthEast
--northWesternmapper _ = error "Wie kommt, da diese Richtung rein?"

-- mapper control function
mapper :: Maybe Dir -> Dir -> Dir
mapper (Just North) dir  = northernMapper dir
mapper (Just East) dir = easternMapper dir
mapper (Just South) dir = southernMapper dir
mapper (Just West) dir = westernMapper dir
mapper (Just NorthEast) dir = northEasternMapper dir --edge cases
mapper (Just SouthEast) dir = southEasternMapper dir -- edge cases
mapper (Just SouthWest) dir = southWesternMapper dir -- edge cases
mapper (Just NorthWest) dir = northWesternmapper dir -- edge cases
mapper Nothing dir = dir 


checkOutsideWhere :: Pos -> Maybe Dir
checkOutsideWhere p
  | row p > 6 && elem (ord(col p)) [97, 98..102] = Just North
  | row p == 7 && ord(col p) == 103 = Just NorthEast -- corner Edgecase
  | elem (row p) [1,2..6] && ord(col p) > 102 = Just East
  | row p == 0 && ord(col p) == 103 = Just SouthEast -- corner Edgecase
  | row p < 1 && elem (ord(col p)) [97, 98..102] = Just South
  | row p == 0 && ord(col p) == 96 = Just SouthWest -- corner Edgecase
  | ord(col p) < 97 && elem (row p) [1,2..6] = Just West
  | row p == 7 && ord(col p) == 96 = Just NorthWest -- cornerEdgecase
  | otherwise = Nothing

winkelMapperPredicateWhere :: Pos -> Dir -> Maybe Dir
winkelMapperPredicateWhere p d = checkOutsideWhere (performStep p d)

path :: Pos -> Dir -> Int ->  [Pos]
path _ _ (-1) = []
path start dir steps = [start] ++  if isNothing (winkelMapperPredicateWhere start dir)  then   path (performStep start dir) dir (steps-1)
                                        else path (performStep start (mapper (winkelMapperPredicateWhere start dir) dir)) (mapper (winkelMapperPredicateWhere start dir) dir) (steps-1)