module ParSudoku.Sudoku6 where

import qualified Logic.GameLogic as GL
import Data.List (sort, find, permutations, intercalate, (\\),
                 transpose, intersect)
import Data.Maybe (fromMaybe, catMaybes, isNothing)
import Control.Parallel.Strategies
import Data.Monoid ((<>))
import Control.Monad (guard)

type R          = Bool
type Value      = Int                             -- A number in a square. 
type Move       = [Value]                         -- A row in the puzzle.
type Coordinate = Int
type Position   = (Coordinate, Coordinate) 
type Puzzle     = [(Coordinate, Coordinate, Value)]

{-
For the 6x6 puzzle:
    ——— ——— ——— ——— ——— ——— 
 6 | 4 |   | 5 |   |   |   |
    ——— ——— ——— ——— ——— ——— 
 5 |   |   | 1 |   |   | 5 |
    ——— ——— ——— ——— ——— ——— 
 4 | 3 |   |   | 2 | 4 |   |
    ——— ——— ——— ——— ——— ——— 
 3 |   | 4 | 2 |   |   | 3 |
    ——— ——— ——— ——— ——— ——— 
 2 | 2 |   |   | 5 |   |   |
    ——— ——— ——— ——— ——— ——— 
 1 |   |   |   | 3 |   | 6 |
    ——— ——— ——— ——— ——— ——— 

     1   2   3   4   5   6
 -}

size = 6

main :: IO ()
main = putStrLn $ prettyPrint optimalPlay
       {-
        - -- Narrow possible initial plays to those that match the clues.
        -let matchClue' [4,_,5,_,_,_] = True
        -    matchClue' _             = False
        -    -- Even with this many filled in answers, it wont terminate.
        -    testing = [[6,2,1,4,3,5],[3,5,6,2,4,1],[1,4,2,6,5,3]] 
        -    -- Almost all of the puzzle must be filled in.
        -    allPossibleStarts :: [[Move]]
        -    allPossibleStarts = 
        -     (map (:[]) $ filter matchClue' $ permutations [1..size])
        -     -- Find the optimal plays of every possible starting move, in
        -     -- parallel.
        -    results :: [Move]
        -    results = (flip GL.find p) $ 
        -          parMap rdeepseq parOptimalPlay allPossibleStarts
        -if not . p $ results
        -      then putStrLn "Couldn't solve this puzzle."
        -      else putStrLn . prettyPrint $ results
        -return ()
        -}

-- Change a set of rows of values to a map. 
movesToPuzzle :: [Move] -> Puzzle
movesToPuzzle = zip3 xs ys . concat
    where coords = do
                   y <- [1..size]
                   x <- [1..size]
                   return (x, y)
          (xs, ys) = unzip coords

-- Filter is slow O(n), need better technique.
checkPuzzle :: Puzzle -> Bool
checkPuzzle xs = 
        all (== allNums) (map sort rows) &&
        all (== allNums) (map sort cols) &&
        all (== allNums) (map sort grids)
    where rows, cols :: [[Value]]
          rows = do -- Get every row in the puzzle 
              y <- [1..size]
              let ls = filter (\(_,y',_) -> y' == y) xs
              return $ map (\(_,_,n) -> n) ls
          cols = do -- Get every column in the puzzle 
              x <- [1..size]
              let ls = filter (\(x',_,_) -> x' == x) xs
              return $ map (\(_,_,n) -> n) ls
          grids :: [[Value]] -- Get a list of every grid's value 
          grids = getGrids size xs
          allNums = [1..size] -- The set of numbers to match. 
          -- Change a list of positions into a list of grids of values.
          getGrids :: Int -> Puzzle -> [[Value]]
          getGrids size xs = gridify $ cellValues xs
              -- Turn a list of values ordered by their places in each grid
              -- square into a list of lists, consisting of each grid square.
              where gridify :: [Value] -> [[Value]] 
                    gridify [] = []
                    gridify xs = take size xs : gridify (drop size xs)
                    -- Change a list of positions to a list of values, 
                    -- ordered by each grid square they appear in.
                    cellValues :: Puzzle -> [Value]
                    cellValues xs = do
                        y' <- [1..3]
                        x' <- [1..2]
                        x  <- [1..3]
                        y  <- [1..2]
                        let x'' = x + (pred x' * 3)
                            y'' = y + (pred y' * 2)
                        return $ getValue (x'', y'') xs
                    getValue :: (Coordinate, Coordinate) -> Puzzle -> Value
                    getValue _ []          = undefined
                    getValue (x, y) ps = 
                      let (_,_,v) = fromMaybe undefined $ 
                            find (\(x',y',_) -> x == x' && y == y') ps
                      in  v

valid :: Puzzle -> R
valid = checkPuzzle

-- Must also match the puzzle's inital clues.
p :: [Move] -> R
p ms = valid (movesToPuzzle ms) && matchClue ms

pPar :: [Move] -> [Move] -> R
pPar preceding ms = valid (movesToPuzzle ms') && matchClue ms'
    where ms' = preceding ++ ms

matchClue :: [Move] -> Bool
--           u v w x y z
matchClue  [[4,_,5,_,_,_],
            [_,_,1,_,_,5],
            [3,_,_,2,4,_],
            [_,4,2,_,_,3],
            [2,_,_,5,_,_],
            [_,_,_,3,_,6]] = True
matchClue  _               = False

optimalPlay :: [Move]
optimalPlay = GL.bigotimes epsilons p

-- Each move (a row in the puzzle) is a permutation of [1..n] for n sized
-- puzzles. Also filtered the permutation to the only possible number 
-- sequences for faster results (otherwise, doesn't terminate).
epsilons :: [[Move] -> GL.J R Move]
epsilons = replicate size epsilon
    where epsilon h = GL.find (possibilities `GL.setMinus` h)
          possibilities = filter condition $ permutations [1..size] 
          {-possibilities = permutations [1..4] -}
          condition [u,v,w,x,y,z] =
              (u == 4 && v == 3 && w == 5 && x == 1 && y == 6 && z == 2)
           || (u == 6 && v == 2 && w == 1 && x == 4 && y == 3 && z == 5)
           || (u == 3 && v == 5 && w == 6 && x == 2 && y == 4 && z == 1)
           || (u == 1 && v == 4 && w == 2 && x == 6 && y == 5 && z == 3)
           || (u == 2 && v == 6 && w == 3 && x == 5 && y == 1 && z == 4)
           || (u == 5 && w == 4 && x == 3)

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
