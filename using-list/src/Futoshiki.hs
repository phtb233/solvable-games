module Futoshiki where
import qualified Logic.GameLogic as GL
import Data.List (sort, find, permutations, intercalate)
import Data.Maybe (fromMaybe)

{-
-
- Sudoku variant, which also includes inequality constraints between
- particular cells (less than/ greater than).
-
For the 4x4 puzzle:

     1   2   3   4 
    ——— ——— ——— ——— 
 1 |   |   |   <   |
    —^— ——— ——— ——— 
 2 |   | 1 |   |   |
    —^— ——— ——— ——— 
 3 |   |   >   |   |
    ——— ——— ——— ——— 
 4 | 1 |   |   |   |
    ——— ——— ——— ——— 
 -}

type R          = Bool
type Value      = Int
type Move       = [Value]
type Coordinate = Int
type Position   = (Coordinate, Coordinate, Value)
-- A triple containing a comparison function and the locations of its
-- left and right operands.
type Constraint =  (Value -> Value -> Bool, 
                   (Coordinate, Coordinate),
                   (Coordinate, Coordinate))
type Puzzle     = [Position]

-- Change a set of rows of values to a set of tuples, including their
-- x and y positions (from bottom left of the grid to top right).
movesToPuzzle :: [Move] -> Puzzle
movesToPuzzle = zip3 xs ys . concat
    where coords = do
                   y <- [1..4]
                   x <- [1..4]
                   return (x, y)
          (xs, ys) = unzip coords

-- Check the rows and columns to make sure they enumerate the full range of
-- numbers [1..5].
checkPuzzle :: Puzzle -> Bool
checkPuzzle xs =
        all (== allChars) (map sort rows) &&
        all (== allChars) (map sort cols)
    where size = 4
          rows, cols :: [[Value]]
          rows = do 
              y <- [1..size]
              let ls = filter (\(_,y',_) -> y' == y) xs
              return $ map (\(_,_,c) -> c) ls
          cols = do -- Get every column in the puzzle 
              x <- [1..size]
              let ls = filter (\(x',_,_) -> x' == x) xs
              return $ map (\(_,_,c) -> c) ls
          allChars = [1..4]

getValue :: (Coordinate, Coordinate) -> Puzzle -> Value
getValue _ []          = undefined
getValue (x, y) ps = 
        let (_,_,v) = fromMaybe undefined $ 
             find (\(x',y',_) -> x == x' && y == y') ps
        in  v

constraints :: [Constraint]
constraints = [ ( (<), (3,1), (4,1) )
              , ( (>), (1,2), (1,1) )
              , ( (>), (1,3), (1,2) )
              , ( (>), (2,3), (3,3) ) ]

matchConstraints :: [Constraint] -> [Move] -> Bool
matchConstraints cs ms = foldl comparison True cs
    where comparison :: Bool -> Constraint -> Bool
          comparison bool c = let (comparedTo, a, b) = c
                                  value :: (Coordinate, Coordinate) -> Value
                                  value = flip getValue (movesToPuzzle ms)
                              in  bool && value a `comparedTo` value b

-- The puzzle must match the given clues.
matchClues :: [Move] -> Bool
matchClues [[_,_,_,_]
           ,[_,1,_,_]
           ,[_,_,_,_]
           ,[1,_,_,_]] = True
matchClues _           = False

valid :: Puzzle -> R
valid = checkPuzzle

p :: [Move] -> R
p ms = valid (movesToPuzzle ms) && matchConstraints constraints ms 
       && matchClues ms

optimalPlay :: [Move]
optimalPlay = GL.bigotimes epsilons p

epsilons :: [[Move] -> GL.J R Move]
epsilons = replicate 4 epsilon
   where epsilon h = GL.find (possibilities `GL.setMinus` h)
         possibilities = permutations [1..4]

main = putStrLn $ prettyPrint optimalPlay

-- Display puzzles in a human readable format.
prettyPrint :: [Move] -> String
prettyPrint ms = 
        -- The top, and the horizontal lines for each row...
        divider ++ intercalate divider  
        -- Put bars '|' at the sides, and separate each element with one.
        (map ( (\s -> "| " ++ s ++ " |") . intercalate " | " . 
        -- Turn each number into a string
        map  show )  ms ) ++ divider
    where divider = " \n " ++ unwords (replicate n "———") ++  " \n"
          n = 4
