module Nonograms where
import qualified Logic.GameLogic as GL
import Data.List (sort, find, permutations, intercalate, transpose, nub, (\\))
import Data.Maybe (fromMaybe)
import Control.Monad(guard, void)

{-
-
- Another Sudoku variant, which simply paints in cells depending on the
- clues lining the edges of the grid.
-
For the 5x10 puzzle:
       (4)     (1) (1)
       (1) (1) (2) (2) (2)    
       (2) (5) (2) (1) (3)
       ——— ——— ——— ——— ———  
   (4)|   |   |   |   |   |
       ——— ——— ——— ——— ———
(1)(1)|   |   |   |   |   |
       ——— ——— ——— ——— ———
(1)(1)|   |   |   |   |   |
       ——— ——— ——— ——— ———
   (1)|   |   |   |   |   |
       ——— ——— ——— ——— ———
      |   |   |   |   |   |
       ——— ——— ——— ——— ———
(2)(1)|   |   |   |   |   |
       ——— ——— ——— ——— ———
   (5)|   |   |   |   |   |
       ——— ——— ——— ——— ———
(1)(2)|   |   |   |   |   |
       ——— ——— ——— ——— ———
   (3)|   |   |   |   |   |
       ——— ——— ——— ——— ———
   (4)|   |   |   |   |   |
       ——— ——— ——— ——— ———
 -}

type R          = Bool
type Value      = Bool
type Move       = [Value]
type X          = Int
type Y          = Int
type Position   = (X, Y, Value)
type Puzzle     = [Position]

width  = 5
height = 5
clues :: [[Clue]]
clues  = fiveByFive

-- Represents the clues that surround the puzzle, which indicate the number
-- of cells in that row/column that should be marked True. The first data
-- constructor is for contiguous clues (all the cells should touch),
-- whereas Seg is for segregated clues (should have some gap between each
-- part).
data Clue = Cont { getCont :: Int }
          |  Seg { getSeg :: [Int] } deriving (Eq, Show)

movesToPuzzle :: [Move] -> Puzzle
movesToPuzzle = zip3 xs ys . concat
    where (xs, ys) = unzip [(x, y) | y <- [1..height], x <- [1..width]]
          
-- Safe list access.
maybeAt :: [a] -> Int -> Maybe a
maybeAt ls n
    | n <= pred (length ls) && n >= 0 = Just $ ls !! n
    | otherwise                       = Nothing

-- The clues that must be matched (as shown above). 
-- First list is row clues, the second is column clues.
fiveByTen = [ [ Cont 4, Seg [1,1], Seg [1,1], Cont 1, Cont 0,
                Seg [2,1], Cont 5, Seg [1,2], Cont 3, Cont 4]
            , [ Seg [4,1,2], Seg[1,5], Seg[1,2,2], Seg [1,2,1], Seg [2,3] ] ]

fiveByFive = [ [ Seg [1,3], Cont 4, Seg [1,3], Cont 2, Cont 1 ]
             , [ Seg [1,1], Cont 1, Cont 3,    Cont 4, Cont 5 ] ]

-- Doesn't terminate.
tenByTen = 
 [[Seg[4,1],Seg[5,3],Cont 4,Seg[3,1],Cont 6,Seg[1,4],Seg[2,3],Cont 3,
   Seg[1,3,1],Seg[3,2]],
  [Seg[6,1],Seg[5,1],Cont 5,Seg[3,2],Seg[2,6],Cont 6,Cont 5,Cont 1,Seg[2,1],
   Seg[1,1,2]]]

-- Check the puzzle matches the clues given at the edges of the grid.
matchClues :: [[Clue]] -> [Move] -> Bool
matchClues cs ms = rows && cols
 where rows, cols :: Bool
       rows = and $ zipWith march (head cs) ms
       cols = and $ zipWith march (last cs) (transpose ms)
            
-- Check if the row was segmented and matches the series of clues.
march :: Clue -> Move -> Bool
march (Seg xs) bools = let (a, b) = march' bools False False 0 [] 
                        in   a && b == xs
-- Make sure the row wasn't segmented, with enough marked cells.
-- Rubbish choice of code; had to use (\\) to get around empty rows.
march (Cont i) bools = let (a, b) = march' bools False False 0 []
                        in   not a && [i] \\ [0] == b
march' :: [Value] -> Bool -> Bool -> Int -> [Int] -> (Bool, [Int])
-- Return a tuple of whether the row was segmented and a list of
-- the marked cells.
march' [] counting seg acc ls = (seg, if acc == 0 then ls else ls ++ [acc])
-- Must keep track of when whenever we start/stop marching over
-- marked cells, counting them as we go.
march' (b:bs) counting seg acc ls
 | b && not counting 
     && not (null ls) = march' bs True     True (acc + 1) ls
 | b && not counting  = march' bs True     seg  (acc + 1) ls
 | b && counting      = march' bs counting seg  (acc+1)   ls
 | not b && counting  = march' bs False    seg   0       (ls ++ [acc])
 | otherwise          = march' bs counting seg   acc      ls

p :: [Move] -> R
p = matchClues clues

optimalPlay :: [Move]
optimalPlay = GL.bigotimes epsilons p

optimalOutcome :: R
optimalOutcome = p optimalPlay

epsilons :: [[Move] -> GL.J R Move]
epsilons = replicate turns epsilon
  where epsilon _ = GL.find $ possibilities
        -- Consists of every possible True/False combination.
        turns   = height -- The number of turns.
        moves   = width  -- The number of moves per turn.
        ps' :: [Move]
        -- One row may be empty. Include a full row of False.
        ps' = nub $ do
            x <- [0..moves] 
            permutations $ take moves $ replicate x True ++ repeat False
        -- Use the clues to narrow the possibilities.
        possibilities :: [Move]
        possibilities = filter (\p -> (any (\f -> f p) tests ) ) ps'
        tests :: [Move -> Bool]
        tests = map march $ concat clues

main = do putStrLn $ prettyPrint clues optimalPlay
          putStrLn $ "Solvable : " ++ show optimalOutcome

-- Human readable output.
prettyPrint :: [[Clue]] -> [Move] -> String
prettyPrint [l, t] ms = 
        let ms' :: [( String, Move )]
            ms' = zip leftSide ms
            -- Indent spaces for the '(' 'x' ')' characters on the
            -- leftside.
            indent :: String
            indent  = replicate (3 * maxLen) ' '
            -- Separate each line of the puzzle.
            divider :: String
            divider = "\n  " ++ indent ++ 
                unwords (replicate (length $ head ms) "———") ++  " \n"
            -- Turn each move (Bool) into an X for true, otherwise empty.
            eachCell :: Move -> String
            eachCell = (\s -> " | " ++ s ++ " |") . 
                         intercalate " | "  . map (\t -> if t then "X"
                                                              else " ")
            isSeg :: Clue -> Bool
            isSeg (Seg _) = True
            isSeg _       = False
            -- Get a segment value from a clue if it exists, as a String.
            safeGetSeg :: Clue -> Int -> String
            safeGetSeg (Seg xs) n = 
                fromMaybe "   " $ xs `maybeAt` n >>= 
                    (\x -> return $ "(" ++ show x ++ ")")
            safeGetSeg _ _ = "    "
            maxLen :: Int -- The size of the largest Seg. 
            maxLen = maximum $ map (\(Seg xs) -> length xs) $ filter isSeg t
            -- The clues displayed on the top and left of the puzzle.
            topSide, leftSide :: [String]
            topSide = do
                y <- [0..maxLen - 1]
                let line = do 
                     clue <- t
                     if (y < maxLen - 1 && isSeg clue) || isSeg clue
                         then return $ safeGetSeg clue (
                                        length(getSeg clue) - (maxLen - y)) ++
                                        " "
                         else if y < maxLen - 1 && not (isSeg clue)
                               then return "    "
                               else return $ "(" ++ show (getCont clue) ++ ") "
                return $ (++) "\n  " $ (++) indent $ concat line
            leftSide = do
                clue <- l
                let line = do 
                     y <- [0..maxLen - 1]
                     if (y < maxLen - 1 && isSeg clue) || isSeg clue
                         then return $ safeGetSeg clue (
                                        length(getSeg clue) - (maxLen - y)) 
                         else if y < maxLen - 1 && not (isSeg clue)
                               then return "   "
                               else return $ "(" ++ show (getCont clue) ++ ")"
                return $ concat line
        in
            concat topSide ++
            divider ++ intercalate divider  
            (map (\(left, move) -> left ++ eachCell move  )  ms' ) ++ divider
