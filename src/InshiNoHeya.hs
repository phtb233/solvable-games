module InshiNoHeya where

import qualified Logic.GameLogic as GL
import qualified Logic.Utils as UL
import Data.List (sort, intercalate, permutations, find)
import Data.Map.Strict (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

{-
- Another Sudoku variant. Same rules, but the numbers in each room must
- have a product of the small number in the top left.
- Doesn't terminate.

For a 5x5 puzzle:

     1   2   3   4   5
    ———————————————————
 1 |⁶  |¹⁵     |¹  |¹² |
   |   |———————————|   |
 2 |   |²⁰     |⁸  |   |
   |———————————|   |   |
 3 |¹⁰     |⁶  |   |   |
   |———————|   |———————|
 4 |⁴  |⁴  |   |¹⁵     |
   |   |———|   |———————|
 5 |   |¹  |   |¹⁰     |
    ———————————————————
-}

type R          = Bool
type Value      = Int
type Product    = Int
type Move       = [Value]
type Coordinate = (Int, Int)
type Puzzle     = Map Coordinate Value

{-size = 5-}
size = 4

currentRoom :: [Room]
currentRoom = fourByFour

-- Contains data about a room contents, and what their product should be.
data Room = Room [(Coordinate)] Product deriving (Eq, Show)

movesToPuzzle = UL.movesToPuzzle size size
checkPuzzle   = UL.checkPuzzle size size False
prettyPrint   = UL.prettyPrint size

checkProduct :: [Room] -> Puzzle -> Bool
checkProduct rs ps = foldl checkp True rs
    where checkp :: Bool -> Room -> Bool
          checkp bool (Room cs prod) = 
            let values :: [Value]
                values = map (ps M.!) cs
            in  bool && product values == prod

fourByFour :: [Room]
fourByFour =
    [ Room [(1,1), (2,1)]       8
    , Room [(3,1), (3,2)]       4
    , Room [(4,1), (4,2)]       6
    , Room [(1,2),(1,3),(1,4)]  12
    , Room [(2,2), (2,3)]       3
    , Room [(3,3), (4,3)]       2
    , Room [(2,4), (3,4)]       6
    , Room [(4,4)]              4
    ]

fiveByFive :: [Room]
fiveByFive = 
    [ Room [(1,1), (1,2)]       6
    , Room [(2,1), (3,1)]       15
    , Room [(4,1)]              1
    , Room [(5,1),(5,2),(5,3)]  12
    , Room [(2,2),(3,2)]        20
    , Room [(4,2),(4,3)]        8
    , Room [(1,3),(2,3)]        10
    , Room [(3,3),(3,4),(3,5)]  6
    , Room [(1,4),(1,5)]        4
    , Room [(2,5)]              1
    , Room [(4,4),(5,4)]        15
    , Room [(4,5),(5,5)]        10 
    ]

p :: [Move] -> R
p ms = checkPuzzle ms && checkProduct currentRoom puzzle
    where puzzle = movesToPuzzle ms

optimalPlay :: [Move]
optimalPlay = GL.bigotimes epsilons p

epsilons :: [[Move] -> GL.J R Move]
epsilons = replicate size epsilon
   where epsilon h = GL.find (possibilities `GL.setMinus` h)
         possibilities = sort $ permutations [1..size]

main = putStrLn $ prettyPrint optimalPlay
