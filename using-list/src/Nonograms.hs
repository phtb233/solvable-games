module Nonograms where
import qualified GameLogic as GL
import Data.List (sort, find, permutations, intercalate, transpose, nub)
import Data.Maybe (fromMaybe)
import Control.Monad(guard, void)

{-
-
- Another Sudoku variant, which simply paints in cells depending on the
- clues lining the edges of the grid.
-
For the 5x5 puzzle:

               (1) (1) (2)
       (1) (1) (3) (2) (2)
       ——— ——— ——— ——— ———  
   (3)|   |   |   |   |   |
       ——— ——— ——— ——— ———
   (1)|   |   |   |   |   |
       ——— ——— ——— ——— ———
(1)(1)|   |   |   |   |   |
       ——— ——— ——— ——— ———
   (3)|   |   |   |   |   |
       ——— ——— ——— ——— ———
   (4)|   |   |   |   |   |
       ——— ——— ——— ——— ———
 -}


type R          = Bool
type Value      = Bool
type Move       = [Value]
type Coordinate = Int
type Position   = (Coordinate, Coordinate, Value)
type Puzzle     = [Position]

-- Represents the clues that surround the puzzle, which indicate the number
-- of cells in that row/column that should be marked True. The first value
-- constructor is for contiguous clues (all the cells should touch),
-- whereas Seg is for segregated clues (should have some gap between each
-- part).
data Clue = Cont { getCont :: Int }
          |  Seg { getSeg :: [Int] } deriving (Eq, Show)

movesToPuzzle :: [Move] -> Puzzle
movesToPuzzle = zip3 xs ys . concat
    where coords = do
                   y <- [1..5]
                   x <- [1..5]
                   return (x, y)
          (xs, ys) = unzip coords

-- The clues that must be matched (as shown above).
clues :: [[Clue]]
clues = [ [ Cont 3, Cont 1, Seg [1,1], Cont 3,  Cont 4  ]
         ,[ Cont 1, Cont 1, Seg [1,3], Seg [1,2], Seg [2,2] ] ]

-- Safe list access via index.
maybeAt :: [a] -> Int -> Maybe a
maybeAt ls n
    | n <= pred (length ls) && n >= 0 = Just $ ls !! n
    | otherwise                       = Nothing

-- Check the puzzle matches the clues given at the edges of the grid.
matchClues :: [[Clue]] -> [Move] -> Bool
matchClues cs ms = rows && cols
 where rows, cols :: Bool
       rows = and $ zipWith march (head cs) ms
       cols = and $ zipWith march (last cs) (transpose ms)
       march :: Clue -> [Value] -> Bool
       -- Check if the row was segmented and matches the series of clues.
       march (Seg xs) bools = let (a, b) = march' bools False False 0 [] 
                              in   a && b == xs
       -- Make sure the row wasn't segmented, with enough marked cells.
       march (Cont i) bools = let (a, b) = march' bools False False 0 []
                              in   not a && [i] == b
       march' :: [Value] -> Bool -> Bool -> Int -> [Int] -> (Bool, [Int])
       -- Return a tuple of whether the row was segmented and a list of
       -- the marked cells.
       march' [] counting seg acc ls = (seg, if acc == 0 
                                              then ls 
                                              else ls ++ [acc])
       -- Must keep track of when whenever we start/stop marching over
       -- marked cells, counting them as we go.
       march' (b:bs) counting seg acc ls
        | b && not counting 
            && not (null ls) = march' bs True True (acc + 1) ls
        | b && not counting  = march' bs True seg (acc + 1) ls
        | b && counting      =    march' bs counting seg (acc+1) ls
        | not b && counting  = march' bs False seg 0 (ls ++ [acc])
        | otherwise          = march' bs counting seg acc ls
            
p :: [Move] -> R
p = matchClues clues

optimalPlay :: [Move]
optimalPlay = GL.bigotimes epsilons p

epsilons :: [[Move] -> GL.J R Move]
epsilons = replicate n epsilon
  where epsilon h = GL.find (possibilities `GL.setMinus` h)
        -- Consists of every possible True/False combination.
        n   = 5
        ps' :: [[Bool]]
        ps' = do
            x <- [1..n]
            nub $ permutations $ take n $ replicate x True ++ repeat False
        -- Use the clues to narrow possibilities 
        possibilities :: [[Bool]]
        possibilities = filter (\p -> (any (\c -> c p) conditions))  ps' 
        -- Create a list of predicates to run over every permutation.
        conditions :: [[Bool] -> Bool]
        conditions = map condi (concat clues)
        -- Each predicate tests whether a permutation satisfies a clue.
        condi :: Clue -> [Bool] -> Bool
        condi (Cont i) = (i == ) . length . filter id
        condi (Seg xs) = (sum xs ==) . length . filter id

main = putStrLn $ prettyPrint clues optimalPlay

-- Human readable output.
prettyPrint :: [[Clue]] -> [Move] -> String
prettyPrint [l, t] ms = 
    let ms' :: [( String, Move )]
        ms' = zip leftSide ms
        indent :: String
        indent  = replicate (3 * maxLen) ' '
        divider :: String
        divider = "\n  " ++ indent ++ 
            unwords (replicate (length ms) "———") ++  " \n"
        eachCell :: Move -> String
        eachCell = (\s -> " | " ++ s ++ " |") . 
                        intercalate " | "  . map (\t -> if t then "X"
                                                            else " ")
        isSeg :: Clue -> Bool
        isSeg (Seg _) = True
        isSeg _       = False
        safeGetSeg :: Clue -> Int -> String
        safeGetSeg (Seg xs) n = 
            fromMaybe "   " $ xs `maybeAt` n >>= 
                (\x -> return $ "(" ++ show x ++ ")")
        safeGetSeg _ _ = "    "
        maxLen :: Int
        maxLen = maximum $ map (\(Seg xs) -> length xs) $ filter isSeg t
        topSide, leftSide :: [String]
        topSide = do
            y <- [0..maxLen - 1]
            let line = do 
                    clue <- t
                    if (y < maxLen - 1 && isSeg clue) || isSeg clue
                        then return $ 
                        safeGetSeg clue 
                            (length(getSeg clue) - (maxLen - y)) ++ " "
                        else if y < maxLen - 1 && not (isSeg clue)
                                then return "    "
                                else return $ 
                                        "(" ++ show (getCont clue) ++ ") "
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
                                else return $ 
                                         "(" ++ show (getCont clue) ++ ")"
            return $ concat line
    in
        -- The top, and the horizontal lines for each row...
        concat topSide ++
        divider ++ intercalate divider  
        -- Put bars '|' at the sides, and separate each element with one.
        (map (\(left, move) -> left ++ eachCell move  )  ms' ) ++ divider

