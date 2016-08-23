module Logic.Utils where

-- The purpose of this module was to keep the code for the Sudoku variants DRY,
-- however using maps instead of lists severly slowed them down
-- (possibly from many calls to movesToPuzzle).

import Data.Map.Strict (Map)
import Data.List (nub, delete, intercalate)
import Math.NumberTheory.Powers.Squares (isSquare', integerSquareRoot')
import qualified Data.Map as M

type Value      = Int
type Move       = [Int]
type Coordinate = (Int, Int)
type Unit       = [Coordinate]
type Puzzle     = Map Coordinate Value

-- Taken from Peter Norvig's Sudoku solver: http://norvig.com/sudoku.html 
cross :: [Int] -> [Int] -> [Coordinate]
cross a b = [(a', b') | a' <- a, b' <- b]

squares :: Int -> Int -> [Coordinate]
squares width height = cross rows cols
    where rows = [1..height]
          cols = [1..width]

unitlist :: Int -> Int -> Bool -> [Unit]
unitlist width height includeBoxes = 
    [cross rows [c]   | c <- cols] ++
    [cross [r]  cols  | r <- rows] ++
    [cross rs   cs    | rs <- boxes, cs <- boxes]
    where rows = [1..height]
          cols = [1..width]
          boxes :: [[Int]]
          boxes = if includeBoxes && width == height && isSquare' width
                      then chunks (integerSquareRoot' width) [1..width]
                      else []

units :: Int -> Int -> Bool -> Map Coordinate [Unit]
units w h boxes = 
    M.fromList [(s, [u | u <- unitlist w h boxes, s `elem` u])
                                                        | s <- squares w h]
peers :: Int -> Int -> Bool -> Map Coordinate Unit
peers w h boxes = M.fromList 
                    [(s,(delete s $ nub $ concat $ units w h boxes M.! s))
                                                            | s <- squares w h]

checkPuzzle :: Int -> Int -> Bool -> [Move] -> Bool
checkPuzzle width height checkBoxes moves = 
    let puzzle   = movesToPuzzle width height moves
        peers'   = peers width height checkBoxes
        squares' = squares width height
    in  not $ or $ do
            s <- squares'
            let val      = puzzle  M.! s
                ps       = peers'  M.! s
                peerVals = map (\pos -> puzzle M.! pos) ps
            return $ any ( == val) peerVals

movesToPuzzle :: Int -> Int -> [Move] -> Puzzle
movesToPuzzle width height ms = M.fromList $ zipWith (,) coords $ concat ms 
    where coords = [(x, y) | y <- [1..height], x <- [1..width]] 

-- A generic version of Data.Text.chunksOf. Break a list into chunks of
-- length n.
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n ls = let (l , rest) = splitAt n ls in l : chunks n rest

-- Show sudoku grid in human readable format.
prettyPrint :: Int -> [Move] -> String
prettyPrint size ms = 
    -- The top, and the horizontal lines for each row...
    divider ++ intercalate divider  
    -- Put bars '|' at the sides, and separate each element with one.
    (map ( (\s -> "| " ++ s ++ " |") . intercalate " | " . 
    -- Turn each number into a string
    map  show )  ms ) ++ divider
        where divider = " \n " ++ unwords (replicate size "———") ++  " \n"
