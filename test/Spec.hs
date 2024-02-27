-- #############################################################################
-- ####### YOUR UNIT TESTS                                           ###########
-- ####### Note: execute tests using "stack test deathstacks:units"  ###########
-- #############################################################################
import Test.Hspec
import Board
import Deathstacks

main :: IO ()
main = hspec $ do
     --path functions
     testPathFunction
     testNothingFromMappers
     --testPerformStep
     --testWinkelMapper
     --testCheckOutside
     --testWinkelMapperPredicate
     -- buildBoardfunctions
     testBuildBoard
     testToPlayers
     testCellMapper
     testToCell
     testMaptoStringCells
     testStringToRow
     testAddSpace
     -- validateFEn functions
     testValidateFEN
     --playerWon functions
     testPlayerWon
     -- possibleMoves function
     testPossibleMoves
     -- isValidMove function
     testisValidMove
     -- testListMOves
     testListMoves
     -- auxilary Tests
     someAuxilaryTests




----------------------------------
--   Test the Path function and Friends!
--------------------------------

testPathFunction :: Spec
testPathFunction = describe "evaluate correctness from path function" $ do
          it "path test #1" $ 
               path (Pos 'a' 4) North 4 `shouldBe` ( [(Pos 'a' 4), (Pos 'a' 5), (Pos 'a' 6), (Pos 'a' 5), (Pos 'a' 4)] :: [Pos])
          it "path test #2" $ 
               path (Pos 'a' 6) West 3 `shouldBe` ([(Pos 'a' 6), (Pos 'b' 6), (Pos 'c' 6), (Pos 'd' 6)] :: [Pos])
          it "path test #3 - a lot of diagonals" $
               path (Pos 'c' 5) NorthWest 8 `shouldBe` ([(Pos 'c' 5) , (Pos 'b' 6), (Pos 'a' 5), (Pos 'b' 4), (Pos 'c' 3), (Pos 'd' 2), (Pos 'e' 1), (Pos 'f' 2), (Pos 'e' 3)] :: [Pos])
          it  "path test #3 - a lot of diagonals 2"  $
               path (Pos 'a' 2) NorthEast 6  `shouldBe` ([(Pos 'a' 2), (Pos 'b' 3), (Pos 'c' 4), (Pos 'd' 5), (Pos 'e' 6), (Pos 'f' 5), (Pos 'e' 4)] :: [Pos])
          it "corner case NorthEast" $
               path (Pos 'e' 5) NorthEast 3 `shouldBe` ([(Pos 'e' 5), (Pos 'f' 6), (Pos 'e' 5), (Pos 'd' 4)] :: [Pos])
          it "corner case SouthEast" $
               path (Pos 'e' 2) SouthEast 3 `shouldBe` ([(Pos 'e' 2), (Pos 'f' 1), (Pos 'e' 2), (Pos 'd' 3)] :: [Pos])
          it "corner case SouthWest" $
               path (Pos 'b' 2) SouthWest 2 `shouldBe` ([(Pos 'b' 2), (Pos 'a' 1), (Pos 'b' 2)] :: [Pos])
          it "corner case NorthWest" $
               path (Pos 'b' 5) NorthWest 2 `shouldBe` ([(Pos 'b' 5), (Pos 'a' 6), (Pos 'b' 5)] :: [Pos])
          it "normal straight test West" $
               path (Pos 'b' 5) West 3 `shouldBe` ([(Pos 'b' 5), (Pos 'a' 5), (Pos 'b' 5), (Pos 'c' 5)] :: [Pos])


testNothingFromMappers :: Spec
testNothingFromMappers = describe "tests the nothing case from the mapper function" $ do
          it "execute Mappper with nothing" $
               mapper Nothing South `shouldBe` (South :: Dir)


{-testPerformStep :: Spec
testPerformStep = describe "evaluate correctness of perform step method" $ do
          it "step to the north" $
               performStep (Pos 'c' 3) North `shouldBe` ((Pos 'c' 4) :: Pos)
          it "step to the NorthEast" $
               performStep (Pos 'c' 3) NorthEast `shouldBe` ((Pos 'd' 4) :: Pos)
          it "step to the East" $
               performStep (Pos 'c' 3) East `shouldBe` ((Pos 'd' 3) :: Pos)
          it "step to the SouthEast" $
               performStep (Pos 'c' 3) SouthEast `shouldBe` ((Pos 'd' 2) :: Pos)
          it "step to the South" $
               performStep (Pos 'c' 3) South `shouldBe` ((Pos 'c' 2) :: Pos)
          it "step to the SouthWest" $
               performStep (Pos 'c' 3) SouthWest `shouldBe` ((Pos 'b' 2) :: Pos)
          it "step to the West" $
               performStep (Pos 'c' 3) West `shouldBe` ((Pos 'b' 3) :: Pos)
          it "step to the NorthWest" $
               performStep (Pos 'c' 3) NorthWest `shouldBe` ((Pos 'b' 4) :: Pos )


testWinkelMapper :: Spec
testWinkelMapper = describe "evaluate correctnesss of winkelMapper function" $ do
          it "map from North" $
               winkelMapper North `shouldBe` (South :: Dir)
          it "map from NorthEast" $
               winkelMapper NorthEast `shouldBe` (SouthWest :: Dir)
          it "map from East" $
               winkelMapper East `shouldBe` (West :: Dir)
          it "map from SouthEast" $
               winkelMapper SouthEast `shouldBe` (NorthWest :: Dir)
          it "map from SouthEast" $
               winkelMapper SouthEast `shouldBe` (NorthWest :: Dir)
          it "map from South" $
               winkelMapper South `shouldBe` (North :: Dir)
          it "map from South" $
               winkelMapper South `shouldBe` (North :: Dir)
          it "map from SouthWest" $
               winkelMapper SouthWest `shouldBe` (NorthEast :: Dir)
          it "map from West" $
               winkelMapper West `shouldBe` (East :: Dir)
          it "map from NorthWest" $
               winkelMapper NorthWest `shouldBe` (SouthEast :: Dir)
          
testCheckOutside :: Spec
testCheckOutside = describe "evaluate correctness of checkOutside function" $ do
          it "column outside of bounds" $
               checkOutside (Pos 'g' 5) `shouldBe` (True :: Bool)
          it "row outside of Bounds" $
               checkOutside (Pos 'c' 7) `shouldBe` (True :: Bool)
          it "row and column outside of bounds" $
               checkOutside (Pos 'g' 7) `shouldBe` (True :: Bool)
          it "row and column in bounds, valid position" $
               checkOutside (Pos 'a' 2) `shouldBe` (False :: Bool)

testWinkelMapperPredicate :: Spec
testWinkelMapperPredicate = describe "evalaute correcntness of winkelMapperPredicate" $ do
          it "winkelmapper is necessary -> true" $
               winkelMapperPredicate (Pos 'a' 6) North 4 `shouldBe` (True :: Bool)
          it "winkelMapper is not necessary -> false" $
               winkelMapperPredicate (Pos 'a' 6) South 4 `shouldBe` (False :: Bool)
-}
               

-- -----------------------------------------
-- Test the BuildBoard Funtion and Friends!!
-- ---------------------------------------

testBuildBoard :: Spec
testBuildBoard = describe "evaluate correctness of BuildBoard function " $ do
          it "Build the correct start board" $
               buildBoard "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` ([[Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]] :: Board)
          it "Build completly empty Board" $
               buildBoard ",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,," `shouldBe` ([[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty]] :: Board)
          it "build random game state from HA PDF" $
               buildBoard "rr,,,,,rr/,,,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,,bbrrrb,bb" `shouldBe` ([[Stack [Red,Red],Empty,Empty,Empty,Empty,Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Stack [Blue,Blue,Red],Stack [Red,Red],Empty,Stack [Red,Red,Blue],Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Empty,Empty,Stack [Blue,Blue,Red,Red,Red,Blue],Stack [Blue,Blue]]] :: Board)


testCellMapper :: Spec
testCellMapper = describe "evalauates the correcntess of cellMapper function (basically the backend of the buildBoard function)" $ do
          it "Build the correct StartBoard but [[Cell]]" $
               cellMapper [["rr","rr","rr","rr","rr","rr"],[" "," "," "," "," "," "],[" "," "," "," "," "," "],[" "," "," "," "," "," "],[" "," "," "," "," "," "],["bb","bb","bb","bb","bb","bb"]] `shouldBe` ([[Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]] :: [[Cell]])



testToCell :: Spec
testToCell = describe "evaluates the correctness of the toCell method from a string" $ do
          it "Build an empty cell from space" $
               toCell " "  `shouldBe` (Empty :: Cell)
          it "Build Stack of Players from rbb" $
               toCell "rbb" `shouldBe` (Stack [Red, Blue, Blue]:: Cell)

testToPlayers :: Spec
testToPlayers = describe "evaluates the correctness of the toPlayers method which builds " $ do
          it "Build stack of Players from this string rbbr" $
               toPlayers "rbbr" `shouldBe` ([Red, Blue, Blue, Red] :: [Player])
          it "Build stack of Players from this string brbr" $
               toPlayers "brbr" `shouldBe` ([Blue, Red, Blue, Red] :: [Player])


testMaptoStringCells :: Spec
testMaptoStringCells = describe "evaluates the correctness of the mapToStringCells [String] -> [[String]] " $ do
          it "some Random FEN-List that is definetly not valid" $
               mapToStringCells ["rr,rr,rr,rr,rr,rr",",,rrb,,,",",,,aa,,","b,,,,,","a,,,,,a","bb,bb,bb,bb,bb,bb"] `shouldBe` ([["rr","rr","rr","rr","rr","rr"],[" "," ","rrb"," "," "," "],[" "," "," ","aa"," "," "],["b"," "," "," "," "," "],["a"," "," "," "," ","a"],["bb","bb","bb","bb","bb","bb"]] :: [[String]])
          it "the start fen list .." $
               mapToStringCells ["rr,rr,rr,rr,rr,rr",",,,,,",",,,,,",",,,,,",",,,,,","bb,bb,bb,bb,bb,bb"] `shouldBe`   ([["rr","rr","rr","rr","rr","rr"],[" "," "," "," "," "," "],[" "," "," "," "," "," "],[" "," "," "," "," "," "],[" "," "," "," "," "," "],["bb","bb","bb","bb","bb","bb"]] :: [[String]])

testStringToRow :: Spec
testStringToRow = describe "evaluates the correctness of the string to row method" $ do
          it "start row red to [[String]]" $ 
               toStringRow "rr,rr,rr,rr,rr,rr" `shouldBe` (["rr","rr","rr","rr","rr","rr"] :: [String])
          it "some random row to [[String]]" $
               toStringRow ",,rrb,,," `shouldBe` ([" "," ","rrb"," "," "," "] :: [String])

testAddSpace :: Spec
testAddSpace = describe "evaluates the correctness of the addSpace method" $ do
          it "add space to the beginning" $
               addSpace ",rr,rr,rr,rr,rr" `shouldBe` (" ,rr,rr,rr,rr,rr" :: String)
          it "add space to the end" $
               addSpace "a,,rrb,,," `shouldBe` ("a,,rrb,,, " :: String)
          it "add to the beginning and to the end" $
               addSpace ",,,,," `shouldBe` (" ,,,,, " :: String)


-- -----------------------------------------
-- Test the BuildBoard Funtion and Friends!!
-- ----------------------------------------

testValidateFEN ::Spec
testValidateFEN = describe "evaluates the correctness of the validateFEN methods. Lets test some" $ do
          it "normal startFEN" $
               validateFEN "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` (True :: Bool)
          it "random gamestate from the HA PDF" $
               validateFEN "rr,,,,,rr/,,,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,,bbrrrb,bb" `shouldBe` (True :: Bool)
          it "empty String from validation tests" $
               validateFEN "" `shouldBe` (False :: Bool)
          it "string to long" $
               validateFEN "rr,rr,rr,rr,rr,rrr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bbb" `shouldBe` (False :: Bool)
          it "string too short" $
               validateFEN "r,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,b" `shouldBe` (False :: Bool)
          it "missing slash" $
               validateFEN "rr,rr,rr,rr,rr,rr/,,,,,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` (False :: Bool)
          it "wrong comma(s)" $
               validateFEN "rr,rr,rr,rr,rr,rr/,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,,bb" `shouldBe` (False :: Bool)
          it "slash at beginning" $
               validateFEN "/rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,b" `shouldBe` (False :: Bool)
          it "slash at the end" $
               validateFEN "rr,rr,rr,rr,rr,r/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb/" `shouldBe` (False :: Bool)
          it "not enough red stones , 1 blue too much" $
               validateFEN "rr,rr,rr,rr,rr,r/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bbb" `shouldBe` (False :: Bool)
          it "not enough blue stones, 1 red stone too much" $
               validateFEN "rr,rr,rr,rr,rr,r/,,,,,/,,,,,/,,,,,/,,,,,/bbb,bb,bb,bb,bb,bb" `shouldBe` (False :: Bool)
          it "empty field should be valid - grading test" $
               validateFEN ",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,," `shouldBe` (True :: Bool)


---------------------------------------------------------
-- test couple of remaining pieces
---------------------------------------------------------
someAuxilaryTests :: Spec
someAuxilaryTests = describe "some auxilary test for code coverage" $ do
          it "toPos Nothing path" $
               toPos (Nothing, 3)  `shouldBe` (Nothing :: Maybe Pos)
          it "test show Red" $
               show Red `shouldBe` ("Red" :: String)
          it "test show Blue" $
               show Blue `shouldBe` ("Blue" :: String)
          it "sho a Pos object" $
               show (Pos 'a' 6) `shouldBe` ("Pos {col = 'a', row = 6}" :: String)
          it "show South" $
               show South `shouldBe` ("South" :: String)
          it "test player equals" $
               Blue == Blue `shouldBe` (True :: Bool)
          it "Test Cell equals" $
               Empty == Empty `shouldBe` (True :: Bool)
          it " test dir equality North" $
               North == North `shouldBe` (True :: Bool)
          it "steps function from Move" $
               steps (Move (Pos 'a'  6) (Pos 'a' 5) 1) `shouldBe` (1 :: Int)
          it "show function from steps" $
               show (Move (Pos 'a'  6) (Pos 'a' 5) 1) `shouldBe` ("a6-1-a5" :: String)
          it "test move equality" $
               (Move (Pos 'a'  6) (Pos 'a' 5) 1) == (Move (Pos 'a'  6) (Pos 'a' 5) 1) `shouldBe` (True :: Bool)
          it "test putCommas empyt List" $
               putCommas [] `shouldBe` ([] :: String)
          it "test dir equality NorthEast" $
               NorthEast == NorthEast `shouldBe` (True :: Bool)
          it "test dir equality East" $
               East == East `shouldBe` (True :: Bool)
          it "test SouthEast equality" $
               SouthEast == SouthEast `shouldBe` (True :: Bool)
          it "Test South Equality" $
               South == South `shouldBe` (True :: Bool)
          it "Test West Equality" $
               West == West `shouldBe` (True :: Bool)
          it "Test NorthWest Equality" $
               NorthWest == NorthWest `shouldBe` (True :: Bool)
          it "Test SouthWest Equality" $
               SouthWest == SouthWest `shouldBe` (True :: Bool)
          it "test false Equality for two cells" $
               Empty == Stack [Red] `shouldBe` (False :: Bool)
          it "a checkRow Test" $
               checkRow [True, False ,False] 2 == (Just 0, 1) `shouldBe` (False :: Bool)
          it "access the second element" $
              snd (checkRow [True, False ,False] 2) == 2 `shouldBe` (True :: Bool)




-------------------------------------------------------
-- Test the playerWon function and friends
-------------------------------------------------------

startBoard :: Board
startBoard = [[Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]

blueWonBoard :: Board
blueWonBoard = [[Stack [Blue,Red,Red],Stack [Blue,Red,Red],Stack [Blue,Red,Red],Stack [Blue,Red,Red],Stack [Blue,Red,Red],Stack [Blue,Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue],Stack [Blue],Stack [Blue],Stack [Blue],Stack [Blue],Stack [Blue]]]

redWonBoard :: Board
redWonBoard = [[Stack [Red],Stack [Red],Stack [Red],Stack [Red],Stack [Red],Stack [Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Red,Blue,Blue],Stack [Red,Blue,Blue],Stack [Red,Blue,Blue],Stack [Red,Blue,Blue],Stack [Red,Blue,Blue],Stack [Red,Blue,Blue]]]

randomGameState :: Board
randomGameState = [[Stack [Red],Stack [Red],Empty,Stack [Red,Red],Stack [Red],Empty],[Empty,Empty,Stack [Blue,Red,Blue,Red],Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Stack [Red,Red,Blue,Blue],Empty],[Empty,Empty,Empty,Empty,Stack [Red,Red,Red],Empty],[Empty,Empty,Empty,Stack [Blue,Red,Red,Blue],Empty,Empty],[Stack [Blue],Stack [Blue],Stack [Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]

testPlayerWon :: Spec
testPlayerWon = describe "evaluates correctness of the playerWon function" $ do
          it "test the normal start Board" $
               playerWon startBoard `shouldBe` (Nothing :: Maybe Player) 
          it "Blue player won" $
               playerWon blueWonBoard `shouldBe` (Just Blue :: Maybe Player)
          it "Red player Won" $
               playerWon redWonBoard `shouldBe` (Just Red :: Maybe Player)
          it "random game state, nobody Won" $
               playerWon randomGameState `shouldBe` (Nothing :: Maybe Player)
          it "test not even possible game state board, but nobody should win"$
               playerWon randomGameStateBoard `shouldBe` (Nothing :: Maybe Player)



--------------------------------------------------------
-- test possibleMoves function and friends
--------------------------------------------------------
testPossibleMoves :: Spec
testPossibleMoves = describe "evaluates correctness of the possible moves function" $ do
          it "test 2 steps from central position" $
               possibleMoves (Pos 'c' 2) (Stack [Red, Red]) `shouldMatchList` ([Move (Pos 'c' 2) (Pos 'c' 3) 1, Move (Pos 'c' 2) (Pos 'c' 4) 2, Move (Pos 'c' 2) (Pos 'd' 3) 1, Move (Pos 'c' 2) (Pos 'e' 4) 2, Move (Pos 'c' 2) (Pos 'd' 2) 1, Move (Pos 'c' 2) (Pos 'e' 2) 2, Move  (Pos 'c' 2) (Pos 'd' 1) 1, Move (Pos 'c' 2) (Pos 'c' 1) 1, Move (Pos 'c' 2) (Pos 'b' 1) 1, Move (Pos 'c' 2) (Pos 'a' 2) 2, Move (Pos 'c' 2) (Pos 'b' 2) 1, Move (Pos 'c' 2) (Pos 'b' 3) 1, Move (Pos 'c' 2) (Pos 'a' 4) 2 ] :: [Move])
          it "1 step from in the NorthEastern border" $
               possibleMoves (Pos 'f' 6) (Stack [Red]) `shouldMatchList` ([Move (Pos 'f' 6) (Pos 'f' 5) 1, Move (Pos 'f' 6) (Pos 'e' 5) 1, Move (Pos 'f' 6) (Pos 'e' 6) 1 ] :: [Move])
          




------------------------------------------------------
-- tst isValidMove function and friends
------------------------------------------------------

tooTallBoard :: Board
tooTallBoard = [[Stack [Red,Red,Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack[Red, Blue, Blue,  Red, Blue],Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]

randomGameStateBoard :: Board
randomGameStateBoard = [[Stack [Red,Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack[Red, Blue, Blue,  Red, Blue],Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]

veryTooTallBoard :: Board
veryTooTallBoard = [[Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red],Stack [Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack[Red, Red, Red, Blue, Blue,  Red, Blue],Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue],Stack [Blue,Blue]]]


testisValidMove :: Spec 
testisValidMove = describe "evaluates correcntness of isValidMoves function" $ do
          it "startBoard with normal move" $
               isValidMove startBoard (Move (Pos 'a' 6) (Pos 'a' 4) 2) `shouldBe` (True :: Bool)
          it "a board with a too tall stack should be moved..." $
               isValidMove tooTallBoard (Move (Pos 'b' 3) (Pos 'a' 3) 1) `shouldBe` (False :: Bool)
          it "too tall stack will be moved but no change of state" $
               isValidMove tooTallBoard (Move (Pos 'a' 3) (Pos 'a' 3) 4) `shouldBe` (False :: Bool)
          it "there is a stack too tall but move from another position" $
               isValidMove tooTallBoard (Move (Pos 'a' 6) (Pos 'a' 3) 3) `shouldBe` (False :: Bool)
          it "another random game state but wants to go from empty cell" $
               isValidMove randomGameStateBoard (Move (Pos 'a' 5) (Pos 'a' 4)1) `shouldBe` (False :: Bool)
          it "right stack and cell moved but noch enough stones taken - so stack would remain too tall" $
               isValidMove veryTooTallBoard (Move (Pos 'a' 3) (Pos 'a' 4)1) `shouldBe` (False :: Bool)
          it "large too tall stack - but we will diminish it" $
               isValidMove veryTooTallBoard (Move (Pos 'a' 3) (Pos 'a' 6)3) `shouldBe` (True :: Bool)


redCannotMoveBoard :: Board
redCannotMoveBoard = [[Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red],Stack [Blue,Blue,Red,Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty]]

blueCannotMoveBoard :: Board
blueCannotMoveBoard = [[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Red,Blue,Red,Red],Stack [Red,Blue,Red,Red],Stack [Red,Blue,Red,Red],Stack [Red,Blue,Red,Red],Stack [Red,Blue,Red,Red],Stack [Red,Blue,Red,Red]]]

redOnlyTwoStones :: Board
redOnlyTwoStones = [[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Stack [Red],Empty,Empty,Empty,Empty,Stack [Red]]]

blueOnlyTwoStones :: Board
blueOnlyTwoStones = [[Stack [Blue],Empty,Empty,Empty,Empty,Stack [Blue]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty]]

redThreeStonesInTheMiddle :: Board
redThreeStonesInTheMiddle = [[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Stack [Red, Red, Red],Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty]]

twoStonesSomeWhere :: Board
twoStonesSomeWhere = [[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Stack[Blue]],[Empty,Empty,Empty,Empty,Empty,Stack[Red]],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty]]

tuchFühlungTooTall :: Board
tuchFühlungTooTall = [[Stack [Red, Red, Red, Red, Red, Red],Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty]]

testListMoves :: Spec
testListMoves = describe "evaluates th correctness of the ListMoves function" $ do
          it "red cannot move" $ 
              listMoves redCannotMoveBoard Red `shouldMatchList` ([] :: [Move])
          it "blue cannot move" $
               listMoves blueCannotMoveBoard Blue `shouldMatchList` ([] :: [Move])
          it "red only two stones" $
               listMoves redOnlyTwoStones Red `shouldMatchList` ([Move (Pos 'a' 1) (Pos 'a' 2) 1, Move (Pos 'a' 1) (Pos 'b' 2) 1, Move (Pos 'a' 1) (Pos 'b' 1)1, Move (Pos 'f' 1) (Pos 'f' 2) 1, Move (Pos 'f' 1) (Pos 'e' 1) 1, Move (Pos 'f' 1) (Pos 'e' 2) 1] :: [Move])
          it "blue only two stones" $
               listMoves blueOnlyTwoStones Blue `shouldMatchList` ([Move (Pos 'a' 6) (Pos 'b' 6) 1, Move (Pos 'a' 6) (Pos 'b' 5) 1, Move (Pos 'a' 6) (Pos 'a' 5) 1, Move (Pos 'f' 6) (Pos 'f' 5)1, Move (Pos 'f' 6) (Pos 'e' 5) 1, Move (Pos 'f' 6) (Pos 'e' 6) 1] :: [Move])
          it "three Red Stones in the Middle" $
               listMoves redThreeStonesInTheMiddle Red `shouldMatchList` ([Move (Pos 'd' 3) (Pos 'd' 4)1, Move (Pos 'd' 3) (Pos 'd' 5)2, Move (Pos 'd' 3) (Pos 'd' 6)3 , Move (Pos 'd' 3) (Pos 'e' 4) 1, Move (Pos 'd' 3) (Pos 'f' 5) 2, Move (Pos 'd' 3) (Pos 'e' 6) 3, Move (Pos 'd' 3) (Pos 'e' 3)1, Move (Pos 'd' 3) (Pos 'f' 3)2, Move (Pos 'd' 3) (Pos 'e' 3)3, Move (Pos 'd' 3) (Pos 'e' 2) 1, Move (Pos 'd' 3) (Pos 'f' 1) 2,  Move (Pos 'd' 3) (Pos 'e' 2) 3, Move (Pos 'd' 3) (Pos 'd' 2)1, Move (Pos 'd' 3) (Pos 'd' 1)2, Move (Pos 'd' 3) (Pos 'd' 2) 3, Move (Pos 'd' 3) (Pos 'c' 2) 1, Move (Pos 'd' 3) (Pos 'b' 1)2, Move (Pos 'd' 3) (Pos 'a' 2)3, Move (Pos 'd' 3) (Pos 'c' 3)1, Move (Pos 'd' 3) (Pos 'b' 3)2, Move (Pos 'd' 3) (Pos 'a' 3)3, Move (Pos 'd' 3) (Pos 'c' 4) 1, Move (Pos 'd' 3) (Pos 'b' 5) 2, Move (Pos 'd' 3) (Pos 'a' 6) 3] :: [Move])
          it "two stones somewhere - Blue" $
               listMoves twoStonesSomeWhere Blue `shouldMatchList` ([Move (Pos 'f' 5) (Pos 'f' 6)1, Move (Pos 'f' 5) (Pos 'f' 4)1, Move (Pos 'f' 5) (Pos 'e' 4)1, Move (Pos 'f' 5) (Pos 'e' 5)1, Move (Pos 'f' 5) (Pos 'e' 6)1  ] :: [Move])
          it "two stones somewhere - Red" $
               listMoves twoStonesSomeWhere Red `shouldMatchList` ([Move (Pos 'f' 4) (Pos 'f' 5)1, Move (Pos 'f' 4) (Pos 'f' 3)1,Move (Pos 'f' 4) (Pos 'e' 3)1, Move (Pos 'f' 4) (Pos 'e' 4)1, Move (Pos 'f' 4) (Pos 'e' 5)1] :: [Move])
          it "test too tall regel mit 1er steps sind verboten wegen 6er Stack" $
               listMoves tuchFühlungTooTall Red  `shouldMatchList` ([Move (Pos 'a' 6) (Pos 'c' 6)2, Move (Pos 'a' 6) (Pos 'd' 6)3 , Move (Pos 'a' 6) (Pos 'e' 6)4, Move (Pos 'a' 6) (Pos 'f' 6)5, Move (Pos 'a' 6) (Pos 'e' 6)6, Move (Pos 'a' 6) (Pos 'c' 4)2, Move (Pos 'a' 6) (Pos 'd' 3)3, Move (Pos 'a' 6) (Pos 'e' 2) 4, Move (Pos 'a' 6) (Pos 'f' 1)5, Move (Pos 'a' 6) (Pos 'e' 2) 6, Move (Pos 'a' 6) (Pos 'a' 4)2, Move (Pos 'a' 6) (Pos 'a' 3)3 , Move (Pos 'a' 6) (Pos 'a' 2)4, Move (Pos 'a' 6) (Pos 'a' 1)5, Move (Pos 'a' 6) (Pos 'a' 2)6] :: [Move])
          it "blau sollte gar nichts ziehen dürfen" $
               listMoves tuchFühlungTooTall Blue `shouldMatchList` ([] :: [Move])