module InshiNoHeya where
import qualified Logic.GameLogic as GL
import qualified Logic.Utils as UL
import Data.List (sort, intercalate, permutations, find)
import Data.Maybe (fromMaybe)

{-
- Another Sudoku variant. Same rules, but the numbers in each room must
- have a product of the superscript number in the top left.
- Doesn't terminate.

For the 5x5 puzzle:

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
type Coordinate = Int
type Position   = (Coordinate, Coordinate, Value)
type Puzzle     = [Position]

size = 5

-- Contains data about a room contents, and what their product should be.
data Room = Room [(Coordinate, Coordinate)] Product deriving (Eq, Show)

movesToPuzzle :: [Move] -> Puzzle
movesToPuzzle = zip3 xs ys . concat
    where (xs,ys) = unzip [(x, y) | y <- [1..size],  x <- [1..size]]

-- Check the rows and columns to make sure they enumerate the full range of
-- numbers [1..5].
checkPuzzle :: Puzzle -> Bool
checkPuzzle xs =
        all (== allChars) (map sort rows) &&
        all (== allChars) (map sort cols)
    where rows, cols :: [[Value]]
          rows = do 
              y <- [1..size]
              let ls = filter (\(_,y',_) -> y' == y) xs
              return $ map (\(_,_,c) -> c) ls
          cols = do -- Get every column in the puzzle 
              x <- [1..size]
              let ls = filter (\(x',_,_) -> x' == x) xs
              return $ map (\(_,_,c) -> c) ls
          allChars = [1..size]

getValue :: (Coordinate, Coordinate) -> Puzzle -> Value
getValue _ []          = undefined
getValue (x, y) ps = 
        let (_,_,v) = fromMaybe undefined $ 
             find (\(x',y',_) -> x == x' && y == y') ps
        in  v

valid :: Puzzle -> R
valid = checkPuzzle

checkProduct :: [Room] -> Puzzle -> Bool
checkProduct rs ps = foldl checkp True rs
    where checkp :: Bool -> Room -> Bool
          checkp bool (Room cs prod) = 
            let values :: [Value]
                values = map (`getValue` ps) cs
            in  bool && product values == prod

rooms :: [Room]
rooms = [ Room [(1,1), (1,2)]       6,
          Room [(2,1), (3,1)]       15,
          Room [(4,1)]              1,
          Room [(5,1),(5,2),(5,3)]  12,
          Room [(2,2),(3,2)]        20,
          Room [(4,2),(4,3)]        8,
          Room [(1,3),(2,3)]        10,
          Room [(3,3),(3,4),(3,5)]  6,
          Room [(1,4),(1,5)]        4,
          Room [(2,5)]              1,
          Room [(4,4),(5,4)]        15,
          Room [(4,5),(5,5)]        10 ]

p :: [Move] -> R
p ms = valid puzzle && checkProduct rooms puzzle
    where puzzle = movesToPuzzle ms

optimalPlay :: [Move]
optimalPlay = GL.bigotimes epsilons p

epsilons :: [[Move] -> GL.J R Move]
epsilons = replicate size epsilon
   where epsilon h = GL.find (possibilities `GL.setMinus` h)
         possibilities = sort $ permutations [1..size]

main = putStrLn $ prettyPrint optimalPlay

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
