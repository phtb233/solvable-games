module ConnectN where

import qualified GameLogic as GL
import Data.List (unionBy, intercalate, sortBy, sort, (\\))
import Data.Maybe (isNothing)
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import Control.Parallel.Strategies
import Control.DeepSeq (deepseq, NFData)
import Control.Exception (evaluate)
import Control.Monad (guard)

type R = Int
type Move = Int  
type Coordinate = Int
type Position = (Coordinate, Coordinate)
type Board    = Map Position Player

-- Not terminating for 3x4, or regular Connect 4 settings.
-- About 4s for Connect 3 on (2.13Ghz x2) laptop
width, height, winningAmount :: Int
height =        3
width  =        3
winningAmount = 3

data Player = X | O deriving (Eq, Read, Show, Ord)

takeTurn :: Player -> Move -> Board -> Board
takeTurn p m b = let col = Map.filterWithKey (\(x,_) _ -> x == m) b
                     len = Map.size col
                     in  if len < height then Map.insert (m, len + 1) p b 
                                    else error $ "Over the top."
                                        ++ "\n move : " ++ show m 
                                        ++ "\n board : " ++ show b

-- Convert a list of moves to alternating turns, starting with player X.
movesToBoard :: [Move] -> Board
movesToBoard moves = foldl func Map.empty pandm 
        where pandm = zip players moves 
              func b (p,m) | m > width || m < 1   = error "Move outta bounds."
                           | wins X b || wins O b = b
                           | otherwise            = takeTurn p m b
              players :: [Player] -- Alternating turns of X and O 
              players = X : O : players

value :: Board -> R
value b 
   | wins X b  = 1
   | wins O b  = -1
   | otherwise = 0

-- Check the board vertically, horizontally and diagonally to see if the 
-- player has matched [winningAmount] in a row.
wins :: Player -> Board -> Bool
wins p b = or v || or h || d
    where v, h :: [Bool]
          winningPlay   = replicate winningAmount p
          -- Get all this player's moves.
          actualPlays = Map.filter (\p' -> p' == p) b
          v = do  
              -- Check each column for a vertical match. 
              x <- [1..width]
              let section = Map.elems $
                    Map.filterWithKey (\(x',_) _ -> x' == x) b
              return $ GL.contained winningPlay section
          h = do
              -- Check each row for a horizontal match.
              y <- [1..height]
              let section = Map.elems $ 
                    Map.filterWithKey (\(_,y') _ -> y' == y) b
              return $ GL.contained winningPlay section
              -- Combine the result of checking the left and right
              -- for diagonal matches.
          d :: Bool
          -- ld = descending towards the left corner (/), rd = descending to
          -- the right (\).
          d = or ld || or rd
          ld = do
                -- Check every possible diagonal, right to left
                diff <- [winningAmount - width..height - winningAmount]
                let section = Map.elems $
                        Map.filterWithKey (\(x',y') _ -> 
                            y' == x' + diff) actualPlays
                return $ GL.contained winningPlay section
          rd = do
                diff <- [winningAmount - width..height - winningAmount]
                let section = Map.elems $
                        Map.filterWithKey (\(x',y') _ -> 
                            y' == succ winningAmount - x' + diff) actualPlays
                return $ GL.contained winningPlay section

p :: [Move] -> R
p ms = value (movesToBoard ms)

pPar :: [Move] -> [Move] -> R
pPar preceding ms = value (movesToBoard $ preceding ++ ms)

parEpsilons :: [Move] -> [[Move] -> GL.J R Move]
parEpsilons preceding = take ((width * height) - length preceding) all'
    where all = epsilonX : epsilonO : all
          all' = drop (length preceding) all
          epsilonX h = GL.argsup (poolOfMoves `GL.setMinus` h)
          epsilonO h = GL.arginf (poolOfMoves `GL.setMinus` h)
          poolOfMoves = sort ((concat $ replicate height [1..width])
                          \\ preceding)

parOptimalPlay :: [Move] -> [Move]
parOptimalPlay moves = let result = GL.bigotimes (parEpsilons moves) 
                                        (pPar moves)
                       in moves ++ result

epsilons :: [[Move] -> GL.J R Move]
epsilons = take (height * width) all
       where all = epsilonX : epsilonO : all
             epsilonX h = GL.argsup (poolOfMoves `GL.setMinus` h)
             epsilonO h = GL.arginf (poolOfMoves `GL.setMinus` h)
             -- For some reason, it wont work unless sorted.
             poolOfMoves = sort $ concat $ replicate height [1..width]

optimalPlay :: [Move]
optimalPlay = GL.bigotimes epsilons p

optimalOutcome :: R
optimalOutcome = p optimalPlay

-- Human readable Board.
prettyPrint :: Board -> String
prettyPrint b =
        -- Populate a list of filled/empty spaces for the connect 4 grid.
    let rows :: [Map Position (Maybe Player)]
        rows = do 
            y <- [1..height]
            -- A list of empty spaces we'll combine with the filled ones
            let emptySpaces :: Map Position (Maybe Player) 
                emptySpaces = Map.fromList $ 
                    zipWith (,) (zip [1..width] (repeat y)) (repeat Nothing)
            return $ flip Map.union emptySpaces 
                   $ Map.map Just $
                Map.filterWithKey (\(_,y') _ -> y' == y) b
        line :: String
        line = "\n" ++ concat ( replicate width " ―――") ++ "\n"
    in 
        intercalate line $ map ( (\s -> "  " ++ s) . intercalate " | " . 
         Map.elems . Map.map (\p -> maybe " " show p)) rows

        {-intercalate line $ map ( (\s -> "  " ++ s) . intercalate " | " . -}
            {-map (\(_,_,p) -> maybe " " show p)) (reverse rows)-}

nextMove :: [Move] -> Move
nextMove played = 
        let possibleNextMoves, possiblePlays, results :: [[Move]]
            possibleNextMoves = do 
               x <- [1..width]
               let m = length $ filter (==x) played
               guard (m < 3)
               return [x]
            possiblePlays = map (played ++) possibleNextMoves
            results = parMap rdeepseq parOptimalPlay possiblePlays
            optimalMoves :: [Move]
            optimalMoves = if even (length played)
                               then xwins results
                               else owins results
            optimalMove = head $ optimalMoves \\ played
            xwins ms = GL.argsup ms p
            owins ms = GL.arginf ms p
        in 
            optimalMove

main :: IO ()
main = do
         let allStarts2 = do
                          a <- [1..width]
                          return [a]
             allPossibleStarts = (map (:[]) [1..width])
         let results = parMap rdeepseq parOptimalPlay allPossibleStarts
         let optimalMoves = GL.argsup results p
             message 
                | p optimalMoves == 1 = "win"
                | p optimalMoves == 0 = "draw"
                | otherwise           = "lose"
         putStrLn $ "For a game of Connect" ++ show winningAmount ++
                  " on a " ++ show width ++ "x" ++ show height ++ " grid;"
         putStrLn $ "X " ++ message ++ "s : " ++ show optimalMoves
            where xwins ms = p ms == 1 
