module Sudoku9 where
import qualified GameLogic as GL
import Data.Maybe (fromMaybe, mapMaybe, isJust, fromJust, 
                   catMaybes, isNothing)
import Data.List (sort, find, permutations, intercalate, (\\),
                 transpose, intersect, intersperse)
import qualified Data.Map.Strict as M
import Control.Parallel.Strategies
import Data.Monoid ((<>))
import Control.Monad (guard, liftM)
import Control.Exception (evaluate)

type R          = Bool
type Value      = Int                             -- A number in a square. 
type Move       = [Value]                         -- A row in the puzzle.
type Coordinate = Int
type Position   = (Coordinate, Coordinate) 
type Puzzle     = M.Map Position Value


{-
For the 9x9 puzzle:
    ——— ——— ——— ——— ——— ——— ——— ——— ———
 9 | 4 |   |   |   |   |   | 3 | 9 |   |
    ——— ——— ——— ——— ——— ——— ——— ——— ———
 8 |   | 9 |   | 3 |   | 8 | 5 | 6 | 1 |
    ——— ——— ——— ——— ——— ——— ——— ——— ———
 7 | 8 |   |   |   | 9 |   | 7 |   |   |
    ——— ——— ——— ——— ——— ——— ——— ——— ———
 6 |   |   |   | 9 | 6 | 4 | 1 |   |   |
    ——— ——— ——— ——— ——— ——— ——— ——— ———
 5 | 6 |   |   |   | 2 |   |   |   | 4 |
    ——— ——— ——— ——— ——— ——— ——— ——— ———
 4 |   |   | 4 | 1 | 5 | 7 |   |   |   |
    ——— ——— ——— ——— ——— ——— ——— ——— ———
 3 |   |   | 9 |   | 8 |   |   |   | 5 |
    ——— ——— ——— ——— ——— ——— ——— ——— ———
 2 | 7 | 8 | 2 | 4 |   | 5 |   | 3 |   |
    ——— ——— ——— ——— ——— ——— ——— ——— ———
 1 |   | 6 | 5 |   |   |   |   |   | 7 |
    ——— ——— ——— ——— ——— ——— ——— ——— ———
     1   2   3   4   5   6   7   8   9 
 -}

size = 9

main :: IO ()
main = do
        -- Check if the solution is Just a winning combination, Nothing if not.
        -- Narrow possible initial plays to those that match the clues.
       let matchClue' [4,_,_,_,_,_,3,9,_] = True
           matchClue' _             = False
           -- Almost all of the puzzle must be filled in.
           allPossibleStarts :: [[Move]]
           allPossibleStarts = 
            (map (:[]) $ filter matchClue' $ permutations [1..size])
            -- Find the optimal plays of every possible starting move, in
            -- parallel.
           results :: [Move]
           results = (flip GL.find p) $ 
                 parMap rdeepseq parOptimalPlay allPossibleStarts
       if not . p $ results
             then putStrLn "Couldn't solve this puzzle."
             else putStrLn . prettyPrint $ results
       return ()

-- Change a set of rows of values to a map. 
movesToPuzzle :: [Move] -> Puzzle
movesToPuzzle = foldl insert' M.empty . zip3 xs ys . concat  
    where coords = do
                   y <- [1..size]
                   x <- [1..size]
                   return (x, y)
          (xs, ys) = unzip coords
          insert' m (x,y,val) = M.insert (x,y) val m

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
          gridify :: [a] -> [[a]]
          gridify [] = []
          gridify xs = let sp = splitAt size xs 
                       in  fst sp : gridify (snd sp)
          cellValues :: [Value]
          cellValues = do
              let root = round $ sqrt $ (fromIntegral size :: Double)
              y' <- [1..root]
              x' <- [1..root]
              x  <- [1..root]
              y  <- [1..root]
              let x'' = x + (pred x' * root)
                  y'' = y + (pred y' * root)
              return $ fromJust $ M.lookup (x'',y'') p

valid :: Puzzle -> R
valid = checkPuzzle

-- A valid puzzle (not the answer to this one). 
-- Should test true when checkPuzzle is called.
solution :: [Move]
solution = reverse 
       [[7,5,9,6,8,2,4,3,1],
        [6,4,1,9,3,5,7,8,2],
        [3,2,8,7,1,4,9,6,5],
        [5,1,4,8,9,3,6,2,7],
        [9,7,3,2,6,1,8,5,4],
        [8,6,2,5,4,7,3,1,9],
        [2,9,7,3,5,8,1,4,6],
        [4,8,6,1,2,9,5,7,3],
        [1,3,5,4,7,6,2,9,8]]

-- Must also match the puzzle's inital clues.
-- Reverse the list for matchClue's readability.
p :: [Move] -> R
p ms = valid (movesToPuzzle ms) && matchClue ms

matchClue :: [Move] -> Bool
--           r s t u v w x y z
matchClue  [[4,_,_,_,_,_,3,9,_],
            [_,9,_,3,_,8,5,6,1],
            [8,_,_,_,9,_,7,_,_],
            [_,_,_,9,6,4,1,_,_],
            [6,_,_,_,2,_,_,_,4],
            [_,_,4,1,5,7,_,_,_],
            [_,_,9,_,8,_,_,_,5],
            [7,8,2,4,_,5,_,3,_],
            [_,6,5,_,_,_,_,_,7]] = True
matchClue  _                     = False

optimalPlay :: [Move]
optimalPlay = GL.bigotimes epsilons p

parEpsilons :: [Move] -> [[Move] -> GL.J R Move]
parEpsilons preceding = replicate (size - length preceding) epsilon
    where epsilon h = GL.find (possibilities `GL.setMinus` h)
          possibilities = permutations [1..size] \\ preceding

pPar :: [Move] -> [Move] -> R
pPar preceding ms = valid (movesToPuzzle ms') && matchClue ms'
    where ms' = preceding ++ ms

parOptimalPlay :: [Move] -> [Move]
parOptimalPlay preceding = let result = GL.bigotimes (parEpsilons preceding)
                                                     (pPar preceding)
                           in preceding ++ result

-- Each move (a row in the puzzle) is a permutation of [1..n] for n sized
-- puzzles. Also filtered the permutation to the only possible number 
-- sequences for faster results. This doesn't terminate, regardless of the 
-- filtering.
epsilons :: [[Move] -> GL.J R Move]
epsilons = replicate size epsilon
    where epsilon h = GL.find (possibilities `GL.setMinus` h)
          possibilities = filter condition $ permutations [1..size] 
          --possibilities = permutations [1..n] 
          condition [r,s,t,u,v,w,x,y,z] = 
               (r == 4 && x == 3 && y == 9)
            || (s == 9 && u == 3 && w == 8 && x == 5 && y == 6 && z == 1)
            || (r == 8 && v == 9 && x == 7)
            || (u == 9 && v == 6 && w == 4 && x == 1)
            || (r == 6 && v == 2 && z == 4)
            || (t == 4 && u == 1 && v == 5 && w == 7)
            || (t == 9 && v == 8 && z == 5)
            || (r == 7 && s == 8 && t == 2 && u == 4 && w == 5 && y == 3)
            || (s == 6 && t == 5 && z == 7)

-- Display puzzles in a human readable format.
prettyPrint :: [Move] -> String
prettyPrint ms = 
        -- The top, and the horizontal lines for each row...
        divider ++ intercalate divider  
        -- Put bars '|' at the sides, and separate each element with one.
        (map ( (\s -> "| " ++ s ++ " |") . intercalate " | " . 
        -- Turn each number into a string, and reverse for readability.
        map  show )  ms ) ++ divider
    where divider = " \n " ++ unwords (replicate n "———") ++  " \n"
          n = 9
