module Sudoku where

import qualified Logic.GameLogic as GL
import qualified Logic.Utils as UL
import Data.Maybe (fromMaybe, mapMaybe, isJust, fromJust, 
                   catMaybes, isNothing)
import Data.List 
import Data.Map.Strict (Map)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Control.Monad (guard, liftM)
import Control.Exception (evaluate)
import Control.Parallel.Strategies
import Debug.Trace

type R             = Bool
type Value         = Int                             -- A number in a square. 
type Move          = [Value]                         -- A row in the puzzle.
type Coordinate    = (Int, Int) 
type Clues         = [[Maybe Value]]
type Unit          = [Coordinate]
type Possibilities = [Value]
type Puzzle        = Map Coordinate Value

size :: Int
size = 9

currentClue :: Clues
currentClue = mediumClue

main :: IO ()
main = parFindSolution

parFindSolution :: IO ()
parFindSolution = do
    header
    let allPossibleStarts = map (:[]) $ head $ map getPermutations $ 
                                findPossibilities currentClue
        results = parMap rdeepseq parOptimalPlay allPossibleStarts
        solution = GL.find results (p currentClue)
    putStrLn $ prettyPrint solution

findSolution :: IO ()
findSolution = do
    header
    putStrLn $ prettyPrint optimalPlay

header :: IO ()
header = do
    let numClues = countClues currentClue
        opinion
            | numClues < 28 = "Hard"
            | numClues < 29 = "Medium"
            | otherwise     = "Easy"
    putStrLn $ "Solving a puzzle of " ++ show (countClues currentClue) 
       ++ " clues... [ " ++ opinion ++ " ]"

movesToPuzzle :: [[a]] -> Map Coordinate a
movesToPuzzle = M.fromList . zipWith (,) coords . concat  
    where coords = [(x, y) | y <- [1..size], x <- [1..size]] 

rows, cols :: [Int]
rows   = [1..size]
cols   = [1..size]

-- The other cells in all the units of a particular cell. The cell used as 
-- a key cannot share the same value as any of its peers.
peers       = UL.peers size size True 
checkPuzzle = UL.checkPuzzle size size True
prettyPrint = UL.prettyPrint size

getValue :: Coordinate -> Puzzle -> Value
getValue (x, y) ps = ps M.! (x, y)

-- Must also match the puzzle's inital clues.
p :: Clues -> [Move] -> R
p clues ms =  checkPuzzle ms && matchClue clues ms

-- Take a list of clues and played moves, and check if the played moves
-- match the clues.
matchClue :: Clues -> [Move] -> Bool
matchClue cs ms = M.differenceWith check clues moves == M.empty
    where moves, clues :: Puzzle
          moves = movesToPuzzle ms
          clues = M.map fromJust $ M.filter isJust $ movesToPuzzle cs
          check :: (Value -> Value -> Maybe Value)
          check l r = if l /= r then Just l else Nothing

optimalPlay :: [Move]
optimalPlay = GL.bigotimes epsilons (p currentClue)

parEpsilons :: [Move] -> [[Move] -> GL.J R Move]
parEpsilons preceding = replicate (size - length preceding) epsilon
    where epsilon h = GL.find (possibilities `GL.setMinus` h)
          possibilities = permutations [1..size] \\ preceding

pPar :: [Move] -> [Move] -> R
pPar preceding ms = let ms' = preceding ++ ms in p [] ms' 

parOptimalPlay :: [Move] -> [Move]
parOptimalPlay preceding = 
    let result = GL.bigotimes (parEpsilons' preceding) (pPar preceding)
    in preceding ++ result

-- Attach all the possible moves to each row to be searched by the
-- Selection Monad.
epsilons :: [[Move] -> GL.J R Move]
epsilons = take size epsilon'
    where epsilon' = zipWith (\ep ps -> ep ps) (repeat epsilon) possibilities'
          epsilon pos _  = GL.find pos
          possiblities :: [[[Int]]]
          possiblities = findPossibilities currentClue
          possibilities' :: [[Move]]
          possibilities' = map getPermutations possiblities

parEpsilons' :: [Move] -> [[Move] -> GL.J R Move]
parEpsilons' preceding = take size' $ drop len epsilon'
    where epsilon' = zipWith (\ep ps -> ep ps) (repeat epsilon) possibilities'
          epsilon pos _  = GL.find pos
          possiblities :: [[Possibilities]]
          possiblities = findPossibilities currentClue
          possibilities' :: [[Possibilities]]
          possibilities' = map getPermutations possiblities
          size' = size - len
          len   = length preceding

countClues :: Clues -> Int
countClues = sum . map (length . catMaybes) 

-- Use the possibilities for a row to create a list of valid permutations.
getPermutations :: [Possibilities] -> [Move]
getPermutations [a,b,c,d,e,f,g,h,i] =
    [[a',b',c',d',e',f',g',h',i'] |   
          a' <- a, b' <- b, c' <- c 
        , d' <- d, e' <- e, f' <- f
        , g' <- g, h' <- h, i' <- i
        , length (nub [a',b',c',d',e',f',g',h',i']) == 9 ]

-- Some test clues of varying difficulty.
easyClue :: Clues
easyClue =  -- 35 clues
 [[Just 4,Nothing,Nothing,Nothing,Nothing,Nothing,Just 3, Just 9, Nothing],
 [Nothing,Just 9, Nothing,Just 3, Nothing,Just 8, Just 5, Just 6, Just 1],
 [Just 8, Nothing,Nothing,Nothing,Just 9, Nothing,Just 7, Nothing,Nothing ],
 [Nothing,Nothing,Nothing,Just 9, Just 6, Just 4, Just 1, Nothing,Nothing],
 [Just 6, Nothing,Nothing,Nothing,Just 2, Nothing,Nothing,Nothing,Just 4],
 [Nothing,Nothing,Just 4, Just 1, Just 5, Just 7, Nothing,Nothing,Nothing],
 [Nothing,Nothing,Just 9, Nothing,Just 8, Nothing,Nothing,Nothing,Just 5],
 [Just 7, Just 8, Just 2, Just 4, Nothing,Just 5, Nothing,Just 3, Nothing],
 [Nothing,Just 6, Just 5, Nothing,Nothing,Nothing,Nothing,Nothing,Just 7]]

hardClue = -- 26 clues, doesn't terminate.
 [[Just 8,Nothing,Just 7,Nothing,Nothing,Just 2,Just 4, Nothing, Nothing],
 [Nothing,Nothing, Nothing,Just 9, Nothing,Nothing,Nothing,Nothing,Nothing],
 [Just 6, Just 2,Just 1,Nothing,Nothing,Just 7,Nothing, Nothing,Nothing ],
 [Nothing,Nothing,Nothing,Just 4,Just 7,Nothing,Nothing, Nothing,Just 1],
 [Just 3, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just 8],
 [Just 9,Nothing,Nothing, Nothing, Just 3, Just 8, Nothing,Nothing,Nothing],
 [Nothing,Nothing,Nothing,Just 2,Nothing, Nothing,Just 5,Just 8,Just 3],
 [Nothing,Nothing,Nothing,Nothing,Nothing,Just 3,Nothing,Nothing,Nothing],
 [Nothing,Nothing,Just 3,Just 8,Nothing,Nothing,Just 7,Nothing,Just 6]]

mediumClue = -- 27 clues, doesn't terminate, but 28 does.
 [[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just 6,Just 8,Nothing],
 [Nothing,Nothing,Nothing,Nothing,Just 7,Just 3,Nothing,Nothing,Just 9],
 [Just 3,Nothing,Just 9,Nothing,Nothing,Nothing,Nothing,Just 4,Just 5],
 [Just 4,Just 9,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],
 [Just 8,Nothing,Just 3,Nothing,Just 5,Nothing,Just 9,Nothing,Just 2],
 [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just 3,Just 6],
 [Just 9,Just 6,Nothing,Nothing,Nothing,Nothing,Just 3,Nothing,Just 8],
 [Just 7,Nothing,Nothing,Just 6,Just 8,Nothing,Nothing,Nothing,Nothing],
 [Nothing,Just 2,Just 8,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]]

-- Use constraint propagation to filter possible solutions.
-- Call recursively until no more changes can be made.
-- Consists of 2 stages:
--      1) Remove certain answers from all other peers.
--      2) If a possibility is unique to a peer, make it a certain answer.
constrain :: Map Coordinate Possibilities -> Map Coordinate Possibilities
constrain possibilities = 
    let getLength = sum . M.map length
        initialLength = getLength possibilities
        singletons = M.filter ((== 1) . length) possibilities
        firstStage :: [(Coordinate, Possibilities)] 
                      -> Map Coordinate Possibilities  
                      -> Map Coordinate Possibilities
        firstStage [] pos                 = pos
        firstStage ((k,v):singletons) pos = 
            let cells  = peers M.! k
                newPos = map (\cell -> let oldVal = pos M.! cell
                                           newVal = if length oldVal == 1
                                                        then oldVal
                                                        else oldVal \\ v
                                       in (cell, newVal) ) cells
            in  firstStage singletons $ (M.fromList newPos) `M.union` pos 
        uniqueOccurences pos = do
            y <- [1..9]
            x <- [1..9]
            let cell = (x, y)
                values = pos M.! cell
            guard (length values > 1)
            val <- values
            let pos'   = M.delete cell pos
                others = concat $ map (pos' M.! ) $ peers M.! cell
                unique = val `notElem` others
            guard unique
            return (cell, [val])
        secondStage :: Map Coordinate Possibilities 
                       -> Map Coordinate Possibilities
        secondStage oldPos = (M.fromList $ uniqueOccurences oldPos) `M.union`
                              oldPos
        result = secondStage $ firstStage (M.toList singletons) possibilities
        resultLength = getLength result
    in 
        -- If nothing was changed, return result, otherwise keep going.
        if resultLength == initialLength then result else constrain result

-- Find the possible solutions for each cell of the puzzle by using clues.
findPossibilities :: Clues -> [[Possibilities]]
findPossibilities clues = 
    let clues' :: Map Coordinate (Maybe Int)
        clues' = movesToPuzzle clues
        possibilities :: Map Coordinate Possibilities
        possibilities = M.map clueToValue clues'
                            where clueToValue (Just n) = [n]
                                  clueToValue _        = [1..size]
        possibilities' = map snd $ M.toList $ constrain possibilities
    in
        transpose $ UL.chunks size possibilities'
