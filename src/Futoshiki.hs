module Futoshiki where

import qualified Logic.GameLogic as GL
import qualified Data.Map.Strict as M
import qualified Logic.Utils as UL
import Data.Map.Lazy (Map)
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
type Coordinate = (Int, Int)
-- A triple containing a comparison function and the locations of its
-- left and right operands.
type Constraint = (Int -> Int -> Bool, Coordinate, Coordinate)
type Puzzle     = Map Coordinate Value

size = 4

movesToPuzzle = UL.movesToPuzzle size size
checkPuzzle   = UL.checkPuzzle size size False
prettyPrint   = UL.prettyPrint size

constraints :: [Constraint]
constraints = [ ( (<), (3,1), (4,1) )
              , ( (>), (1,2), (1,1) )
              , ( (>), (1,3), (1,2) )
              , ( (>), (2,3), (3,3) ) ]

matchConstraints :: [Constraint] -> [Move] -> Bool
matchConstraints cs ms = foldl comparison True cs
    where comparison :: Bool -> Constraint -> Bool
          comparison bool c = let (comparedTo, a, b) = c
                                  value :: Coordinate -> Value
                                  value = ((movesToPuzzle ms) M.! )
                              in  bool && value a `comparedTo` value b

-- The puzzle must match the given clues.
matchClues :: [Move] -> Bool
matchClues [[_,_,_,_]
           ,[_,1,_,_]
           ,[_,_,_,_]
           ,[1,_,_,_]] = True
matchClues _           = False

p :: [Move] -> R
p ms = checkPuzzle ms  && matchConstraints constraints ms 
       && matchClues ms

optimalPlay :: [Move]
optimalPlay = GL.bigotimes epsilons p

epsilons :: [[Move] -> GL.J R Move]
epsilons = replicate 4 epsilon
   where epsilon h = GL.find (possibilities `GL.setMinus` h)
         possibilities = permutations [1..4]

main = putStrLn $ prettyPrint optimalPlay
