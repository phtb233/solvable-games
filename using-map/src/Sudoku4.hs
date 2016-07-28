module Sudoku4 where

import qualified GameLogic as GL
import Data.List (sort, find, permutations, intercalate, (\\), transpose,
                  intersect)
import Data.Maybe (fromMaybe, catMaybes, isNothing)
import qualified Data.Map.Strict as M
import Control.Parallel.Strategies
import Debug.Trace(traceShow)
import Data.Monoid ((<>))
import Control.Monad (guard)

type R          = Bool
type Value      = Int                             -- A number in a square. 
type Move       = [Value]                         -- A row in the puzzle.
type Coordinate = Int
type Position   = (Coordinate, Coordinate) 
type Puzzle     = M.Map Position Value

{-
For the 4x4 puzzle:

     1   2   3   4 
    ——— ——— ——— ——— 
 1 |   | 2 |   | 1 |
    ——— ——— ——— ——— 
 2 | 1 |   |   |   |
    ——— ——— ——— ——— 
 3 |   |   |   | 4 |
    ——— ——— ——— ——— 
 4 | 4 |   | 3 |   |
    ——— ——— ——— ——— 
 -}

size = 4

-- The parallelism does have an effect, but the puzzle is too small to
-- benefit from it.
main :: IO ()
main = do
        let matchClue' [_,2, _,1] = True
            matchClue' _          = False
            allPossibleStarts :: [[Move]]
            allPossibleStarts = 
                map (:[]) $ filter matchClue' $ permutations [1..size]
            -- Find optimal plays for all starting moves that match the top
            -- row of the puzzle, using parallelism.
        let results :: [Move]
            results = (flip GL.find p) $ 
                    parMap rdeepseq parOptimalPlay allPossibleStarts
        if not . p $ results
            then putStrLn "I couldn't solve this puzzle."
            else putStrLn . prettyPrint $ results

-- Change a set of rows of values to a set of tuples, including their
-- x and y positions (from bottom left of the grid to top right).
movesToPuzzle :: [Move] -> Puzzle
movesToPuzzle = M.fromList . zipWith (,) coords . concat
    where coords = do
                   y <- [1..size]
                   x <- [1..size]
                   return (x, y)

-- Filter is slow O(n), need better technique.
checkPuzzle :: Puzzle -> Bool
checkPuzzle p = 
                all (== allNums) rows 
             && all (== allNums) cols 
             && all (== allNums) grids
    where rows, cols, grids :: [[Value]]
          rows = do
              y <- [1..size]
              return $ sort $ M.elems $ 
                        M.filterWithKey (\(_,y') _ -> y' == y) p
          cols = do
              x <- [1..size]
              return $ sort $ M.elems $ 
                        M.filterWithKey (\(x',_) _ -> x' == x) p
          allNums = [1..size]
          grids = map sort $ gridify cellValues
          gridify :: [Value] -> [[Value]]
          gridify [] = []
          gridify xs = let sp = splitAt size xs 
                       in  fst sp : gridify (snd sp)
          cellValues :: [Value]
          cellValues = do
              let root = round $ sqrt (fromIntegral size :: Double)
              y' <- [1..root]
              x' <- [1..root]
              x  <- [1..root]
              y  <- [1..root]
              let x'' = x + (pred x' * root)
                  y'' = y + (pred y' * root)
              return $ fromMaybe undefined $ M.lookup (x'',y'') p

valid :: Puzzle -> R
valid = checkPuzzle

-- Must also match the puzzle's inital clues.
p :: [Move] -> R
p ms = valid (movesToPuzzle ms) && matchClue ms

pPar :: [Move] -> [Move] -> R
pPar preceding ms = valid (movesToPuzzle ms') && matchClue ms'
    where ms' = preceding ++ ms

matchClue :: [Move] -> Bool
matchClue  [[_,2,_,1],
            [1,_,_,_],
            [_,_,_,4],
            [4,_,3,_]] = True
matchClue  _           = False

optimalPlay :: [Move]
optimalPlay = GL.bigotimes epsilons p

-- Each move (a row in the puzzle) is a permutation of [1..n] for n sized
-- puzzles. Also filtered the permutation to the only possible number 
-- sequences for faster results (otherwise, doesn't terminate).
epsilons :: [[Move] -> GL.J R Move]
epsilons = replicate size epsilon
    where epsilon h = GL.find (possibilities `GL.setMinus` h)
          possibilities = permutations [1..size] 

parEpsilons :: [Move] -> [[Move] -> GL.J R Move]
parEpsilons preceding = replicate (size - length preceding) epsilon
    where epsilon h = GL.find (possibilities `GL.setMinus` h)
          possibilities = permutations [1..size] \\ preceding

parOptimalPlay :: [Move] -> [Move]
parOptimalPlay preceding = let result = GL.bigotimes (parEpsilons preceding)
                                                     (pPar preceding)
                           in preceding ++ result

-- Display puzzles in a human readable format.
prettyPrint :: [Move] -> String
prettyPrint ms = 
        -- The top, and the horizontal lines for each row...
        divider ++ intercalate divider  
        -- Put bars '|' at the sides, and separate each element with one.
        (map ( (\s -> "| " ++ s ++ " |") . intercalate " | " . 
        -- Turn each number into a string
        map  show )  ms ) ++ divider
    where divider = " \n " ++ unwords (replicate size "———") ++  " \n"
