module SudokuN where

import qualified GameLogic as GL
import Data.Maybe (fromMaybe, mapMaybe, isJust, fromJust, 
                   catMaybes, isNothing)
import Data.List (sort, find, permutations, intercalate, (\\),
                  transpose, intersect, intersperse, nub)
import qualified Data.Map.Strict as M
import Control.Parallel.Strategies
import Data.Monoid ((<>))
import Control.Monad (guard, liftM)
import Control.Exception (evaluate)

{-
    This module experiments with another algorithm built specifically for 
    sudoku puzzles.
-}

type R          = Bool
type Value      = Int                             -- A number in a square. 
type Move       = [Value]                         -- A row in the puzzle.
type Coordinate = Int
type Position   = (Coordinate, Coordinate) 
type Puzzle     = M.Map Position Value
type Possibility = M.Map Position (Maybe [Value])

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
    let foobar :: [Move] -> Maybe [Move]
        foobar moves = if p moves then Just moves else Nothing
        -- Run this check over the list of possibilities in parallel, and 
        -- get a set of winning moves.
        solution :: [[Move]]
        solution = catMaybes(map foobar findSolution `using` parList rseq)
        --solution = catMaybes (map foobar findSolution)
    mapM_ ((>>) (putStrLn "Possible solution:") . mapM_ print) solution
    --mapM_ (print) findSolution

-- Change a set of rows of values to a map. 
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

-- Must also match the puzzle's inital clues.
-- Reverse the list for matchClue's readability.
p :: [Move] -> R
p ms = valid (movesToPuzzle ms)

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

-- Use the numbers in each inner square grid to narrow possible solutions.
gridTest :: [[Move]] -> [[Move]]
gridTest ls = 
    let -- A generic version of Data.Text.chunksOf. Break a list into chunks of
        -- length n.
        chunks :: Int -> [a] -> [[a]]
        chunks _ [] = []
        chunks n ls = let (l , rest) = splitAt n ls
                          in   l : chunks n rest
        threes = chunks 3
        nines  = chunks 9
        unarranged = do
                rows <- threes ls
                foo  <- map concat $ transpose $ map threes rows
                let singletons :: Move
                    singletons = concat $ filter ((==1) . length) foo
                let pruned :: [Move]
                    pruned = map (\ls -> if length ls > 1
                                            then ls \\ singletons
                                            else ls) foo
                return pruned
        almostArranged = threes $ nines $ concat $ map (take 3) unarranged ++
                                   map (take 3 . drop 3) unarranged ++
                                   map (drop 6) unarranged
    in
        map head almostArranged ++ map (flip (!!) 1) almostArranged 
                                ++ map (flip (!!) 2) almostArranged 

-- Same as grid test, but use the values in each row.
rowTest :: [[Move]] -> [[Move]]
rowTest ls = do
        row <- ls
        let singletons :: Move
            singletons = concat $ filter ((==1) . length) row
        let pruned :: [Move]
            pruned = map (\ls -> if length ls > 1
                                    then ls \\ singletons
                                    else ls) row
        return pruned
        
colTest :: [[Move]] -> [[Move]]
colTest = transpose . rowTest . transpose

-- Recursively run tests to narrow the possiblities until no more changes
-- are made.
runTests :: [[Move]] -> [[Move]]
runTests ls = 
        let initialSum = sum $ map (sum . map length) ls
            ls' = rowTest $ colTest $ gridTest ls
            resultSum = sum $ map (sum . map length) ls'
        in if initialSum /= resultSum
               then runTests ls'
               else ls'

-- The given numbers of an incomplete puzzle.
clues :: [[Maybe Int]]
clues = 
 -- Solvable [Easy]
 --[[Just 4,Nothing,Nothing,Nothing,Nothing,Nothing,Just 3, Just 9, Nothing],
 --[Nothing,Just 9, Nothing,Just 3, Nothing,Just 8, Just 5, Just 6, Just 1],
 --[Just 8, Nothing,Nothing,Nothing,Just 9, Nothing,Just 7, Nothing,Nothing ],
 --[Nothing,Nothing,Nothing,Just 9, Just 6, Just 4, Just 1, Nothing,Nothing],
 --[Just 6, Nothing,Nothing,Nothing,Just 2, Nothing,Nothing,Nothing,Just 4],
 --[Nothing,Nothing,Just 4, Just 1, Just 5, Just 7, Nothing,Nothing,Nothing],
 --[Nothing,Nothing,Just 9, Nothing,Just 8, Nothing,Nothing,Nothing,Just 5],
 --[Just 7, Just 8, Just 2, Just 4, Nothing,Just 5, Nothing,Just 3, Nothing],
 --[Nothing,Just 6, Just 5, Nothing,Nothing,Nothing,Nothing,Nothing,Just 7]]
 -- Not solvable [Evil]
 --[[Just 8,Nothing,Just 7,Nothing,Nothing,Just 2,Just 4, Nothing, Nothing],
 --[Nothing,Nothing, Nothing,Just 9, Nothing,Nothing,Nothing,Nothing,Nothing],
 --[Just 6, Just 2,Just 1,Nothing,Nothing,Just 7,Nothing, Nothing,Nothing ],
 --[Nothing,Nothing,Nothing,Just 4,Just 7,Nothing,Nothing, Nothing,Just 1],
 --[Just 3, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just 8],
 --[Just 9,Nothing,Nothing, Nothing, Just 3, Just 8, Nothing,Nothing,Nothing],
 --[Nothing,Nothing,Nothing,Just 2,Nothing, Nothing,Just 5,Just 8,Just 3],
 --[Nothing,Nothing,Nothing,Nothing,Nothing,Just 3,Nothing,Nothing,Nothing],
 --[Nothing,Nothing,Just 3,Just 8,Nothing,Nothing,Just 7,Nothing,Just 6]]
 -- Solvable [Easy]
 --[[Just 2,Nothing,Nothing,Nothing,Just 1,Nothing,Nothing,Nothing,Nothing],
 --[Just 5,Just 1,Nothing,Nothing,Nothing,Just 8,Just 4,Nothing,Nothing],
 --[Just 3,Just 9,Just 8,Just 6,Just 4,Nothing,Just 1,Nothing,Just 7],
 --[Nothing,Nothing,Just 5,Just 2,Nothing,Nothing,Nothing,Nothing,Nothing],
 --[Just 1,Just 6,Nothing,Nothing,Just 5,Nothing,Nothing,Just 4,Just 3],
 --[Nothing,Nothing,Nothing,Nothing,Nothing,Just 4,Just 9,Nothing,Nothing],
 --[Just 8,Nothing,Just 3,Nothing,Just 2,Just 6,Just 7,Just 1,Just 4],
 --[Nothing,Nothing,Just 4,Just 5,Nothing,Nothing,Nothing,Just 8,Just 6],
 --[Nothing,Nothing,Nothing,Nothing,Just 8,Nothing,Nothing,Nothing,Just 9]]
 -- Solvable [Medium] 50secs
 [[Nothing,Just 7,Just 1,Nothing,Nothing,Nothing,Just 5,Nothing,Just 9],
 [Nothing,Just 5,Nothing,Just 6,Nothing,Just 7,Nothing,Just 1,Nothing],
 [Just 2,Nothing,Just 4,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
 [Nothing,Just 6,Nothing,Nothing,Just 8,Nothing,Just 1,Nothing,Nothing],
 [Just 1,Nothing,Nothing,Just 5,Just 6,Just 3,Nothing,Nothing,Just 8],
 [Nothing,Nothing,Just 3,Nothing,Just 9,Nothing,Nothing,Just 5,Nothing],
 [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just 7,Nothing,Just 1],
 [Nothing,Just 2,Nothing,Just 3,Nothing,Just 1,Nothing,Just 6,Nothing],
 [Just 9,Nothing,Just 5,Nothing,Nothing,Nothing,Just 3,Just 4,Nothing]]

-- My own method of finding possible solutions (filters and tests the 
-- possible moves using the given clues).
findSolution :: [[Move]]
findSolution = 
    let -- Get the clues corresponding to the grid's rows/columns as 
        -- lists of possible values.
        rowClues, colClues :: [Move]
        rowClues = map (invert . catMaybes) clues
        colClues = map (invert . catMaybes) (transpose clues)
        -- Flip the numbers that have been given as clues to those that
        -- havent' been given.
        invert :: [Int] -> [Int]
        invert   = ([1..size] \\)
        -- Place the list of possible moves for a row in each space where
        -- a clue hasn't been given.
        possibleRow :: [[Maybe Move]]
        possibleRow = zipWith replaceNothing clues rowClues
        replaceNothing tc rc = map (\mb -> if isNothing mb then Just rc
                                              else Nothing) tc
        -- Apply the restrictions that concern the columns to the
        -- restrictions that concern the rows (possibleRow). This is done
        -- by intersecting the ranges of possible moves for rows and cols.
        possibilities :: [[Maybe Move]]
        possibilities = transpose $ zipWith maybIntersect 
                (transpose possibleRow) colClues
        maybIntersect pr cc = map ( >>= (\r -> Just $ r `intersect` cc)) pr
        -- Combine the given moves with the possible moves.
        combined :: [[Move]]
        combined = map catMaybes $ zipWith joinMaybes possibilities 
                    ( map ( map  (>>= Just . (:[]))) clues)
        joinMaybes comb tc = zipWith (<>) comb tc
        -- Assemble nested lists of all the possibilities for a given Move
        -- in the puzzle. Make sure no values are repeated.
        possibilities' :: [[Move]]
        possibilities' = do
            [r, s, t, u, v, w, x, y, z] <- runTests combined
            return [[r', s', t', u', v', w', x', y', z'] |
             r' <- r, s' <- s, t' <- t, u' <- u, v' <- v, w' <- w, x' <- x,
             y' <- y, z' <- z,length(nub [r',s',t',u',v',w',x',y',z']) == size]
    in 
            -- Iterate over every possible row, considering the given clues.
            let [a, b, c, d, e, f, g, h, i] = possibilities' 
            -- Turn them into sets of puzzles which are then filtered
            -- against p.
            in [[a', b', c', d', e', f', g', h', i'] |
                a' <- a, b' <- b, c' <- c, d' <- d, e' <- e, f' <- f, g' <- g,
                h' <- h, i' <- i]
