module Buchstabensalat where
    
import qualified Logic.GameLogic as GL
import Data.Maybe (isJust, catMaybes, fromMaybe, fromJust)
import Data.List (sort, transpose, permutations, intercalate)
import Control.Monad (guard)

-- Similar to Sudoku. Rules:
-- 1) Each row and column must contain one instance of letters A - C and an
--      empty cell.
-- 2) The first non-empty cell must match the clues given outside the grid.
--      e.g. in the puzzle below, the first row must start with B, and the
--      first column must start with C. The letter at the end of the last
--      row must be A.

{-
 -For the 4x4, A~C puzzle:
     C
    ——— ——— ——— ——— 
 B |   |   |   |   |
    ——— ——— ——— ——— 
   |   |   |   |   |
    ——— ——— ——— ——— 
   |   |   |   |   |
    ——— ——— ——— ——— 
   |   |   |   |   | A
    ——— ——— ——— ——— 
                 C 
 -}

type Value = Maybe Char
type Move = [Value]
type R = Bool
type Coordinate = Int
type Position = (Coordinate, Coordinate, Maybe Char)
type Puzzle  = [Position]

width = 4
height = 4
start = 'A'
finish = 'C'

-- Take a full list of moves and turn them into a list of coordinates and
-- values.
movesToPuzzle :: [Move] -> Puzzle
movesToPuzzle = zip3 xs ys . concat
    where (xs, ys) = unzip [(x, y) | y <- [1..height], x <- [1..width]]

-- Check that each row and column contains a single instance from the range
-- of letters ['A'..'C']
checkPuzzle :: Puzzle -> Bool
checkPuzzle xs =
        all (== allChars) (map sort rows) &&
        all (== allChars) (map sort cols)
    where size = 4 -- 4x4 Grid 
          rows, cols :: [[Value]]
          rows = do 
              y <- [1..size]
              let ls = filter (\(_,y',_) -> y' == y) xs
              return $ map (\(_,_,c) -> c) ls
          cols = do -- Get every column in the puzzle 
              x <- [1..size]
              let ls = filter (\(x',_,_) -> x' == x) xs
              return $ map (\(_,_,c) -> c) ls
          allChars = (Nothing:) $ map Just [start..finish]

-- The clues given outside of the puzzle.
clues :: [Move]
clues = [ [ Just 'B', Nothing, Nothing, Nothing ]    -- Left  side clues 
        , [ Nothing,  Nothing, Nothing, Just 'A' ]   -- Right side clues
        , [ Just 'C', Nothing, Nothing, Nothing ]    -- Top clues
        , [ Nothing,  Nothing, Nothing, Just 'C' ] ] -- Bottom clues

-- Check that the solution matches the clues (the letters outside the puzzle
-- that specify the beginning/ending letters of rows/columns).
matchClue :: [Move] -> [Move] -> Bool
matchClue [l,r,t,b] moves = and left && and right && and top && and bottom
    where left, right, top, bottom :: [Bool]
          rows, cols :: [String]
          l',r',t',b' :: [(Maybe Char, Char)] -- (clue, move) 
          -- Whether clues and moves match.
          check :: [(Maybe Char, Char)] -> [Bool] 
          -- Values in each row, from left to right. The head == left side.
          rows = transpose $ map catMaybes moves
          -- Values in each column. 
          cols = transpose $ map catMaybes $ transpose moves
          l'   = zip l $ head rows
          r'   = zip r $ last rows
          t'   = zip t $ head cols
          b'   = zip b $ last cols
          check ls = do
                 (clue, value) <- ls
                 guard (isJust clue) -- If there's no clue, stop. 
                 return $ fromJust clue == value
          left   = check l'
          right  = check r'
          top    = check t'
          bottom = check b'

valid :: Puzzle -> R
valid = checkPuzzle 

optimalPlay :: [Move]
optimalPlay = GL.bigotimes epsilons p

epsilons :: [[Move] -> GL.J R Move]
epsilons = replicate height epsilon
    where epsilon _ = GL.find possibilities
          possibilities = permutations $ (Nothing:) $ 
                            map Just [start..finish]

p :: [Move] -> R
p ms = valid (movesToPuzzle ms) && matchClue clues ms

main :: IO ()
main = putStrLn $ prettyPrint clues optimalPlay

-- Human readable representation of a given solution and clues.
prettyPrint :: [Move] -> [Move] -> String
prettyPrint [l,r,t,b] ms = 
    let ms' :: [( Maybe Char, Move, Maybe Char)]
        ms' = zip3 l ms r
        divider :: String
        divider = " \n    " ++ unwords (replicate width "―――") ++ " \n"
        eachCell :: Move -> String
        eachCell = (\s -> " | " ++ s ++ " |") . 
                        intercalate " | "  . map ((: []) . fromMaybe ' ')
        eachSide :: String -> Maybe Char -> String
        eachSide s =  (++) s . (: []) . fromMaybe ' '
    in
        "  " ++ concatMap (eachSide "   ") t 
        ++ "    " ++ divider 
        ++ intercalate divider 
        (map (\(left, move, right) -> 
            eachSide " " left ++ eachCell move ++ eachSide " " right) ms') 
        ++ "    " ++ divider
        ++ "  " ++ concatMap (eachSide "   ") b 
