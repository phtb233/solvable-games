module ConnectN where

import qualified Logic.GameLogic as GL
import qualified Logic.Utils as UL
import Data.List (unionBy, intercalate, sortBy, sort, (\\))
import Data.Maybe (isNothing)
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies
import Control.DeepSeq (force, deepseq, NFData)
import Control.Exception (evaluate)

{-
- Supports an arbitrarily sized grid. Typical Connect 4 settings are:
- width         = 7
- height        = 6
- winningAmount = 4
-}

type Player = UL.Player
x = UL.X
o = UL.O

type R          = Int
type Move       = Int  
type X          = Int
type Y          = Int
type Position   = (X, Y, Player)
type Board      = [Position]

width, height, winningAmount :: Int
height =        3
width  =        3
winningAmount = 3


takeTurn :: Player -> Move -> Board -> Board
takeTurn p m b = 
    let col = filter (\(x,_,_) -> x == m) b
        len = length col
    in  if len < height then b ++ [(m, len + 1, p)] 
                else error $ "Over the top."
                    ++ "\n move : " ++ show m 
                    ++ "\n board : " ++ show b

-- Convert a list of moves to alternating turns, starting with player X.
movesToBoard :: [Move] -> Board
movesToBoard moves = foldl func [] pandm 
    where pandm = zip players moves 
          func b (p,m) | m > width || m < 1   = error "Move outta bounds."
                       | wins x b  || wins o b = b
                       | otherwise            = takeTurn p m b
          players :: [Player] -- Alternating turns of X and O 
          players = x : o : players

-- Find the outcome from a board state.
value :: Board -> R
value b 
   | wins x b  = 1
   | wins o b  = -1
   | otherwise = 0

-- Check the board vertically, horizontally and diagonally to see if the 
-- player has won.
wins :: Player -> Board -> Bool
wins p b = or v || or h || or d
    -- Very verbose and clumsy check for a match vertically,
    -- horizontally, and diagonally.
    where v, h :: [Bool]
          winningPlay   = replicate winningAmount p
          -- Get all this player's moves.
          actualPlays = filter (\(_,_,p') -> p' == p) b
          v = do  
              -- Check each column for a vertical match. 
              x <- [1..width]
              let section = map (\(_,_,p') -> p') $ 
                    filter (\(x',_,_) -> x' == x) b
              return $ GL.contained winningPlay section
          h = do
              -- Check each row for a horizontal match.
              y <- [1..height]
              let section = filter (\(_,y',_) -> y' == y) actualPlays
              return $ length section == winningAmount
              -- Combine the result of checking the left and right
              -- for diagonal matches.
          d :: [Bool]
          -- Check both diagonals for matches. ld = descending to the left
          -- corner (/), rd = descending to the right (\)
          d = do
                diff <- [winningAmount - width..height - winningAmount]
                let ld = map (\(_,_,p') -> p') $
                        filter (\(x',y',_) -> y' == x' + diff) actualPlays
                    rd = map (\(_,_,p') -> p') $
                        filter (\(x',y',_) -> y' == succ winningAmount - x' 
                        + diff) actualPlays
                return $ GL.contained winningPlay ld ||
                         GL.contained winningPlay rd

-- Attempt at a more efficient check for winning game boards. Checks the
-- neighbours of the most recently played pieces.
{-
 -wins :: Player -> Board -> Position -> Bool
 -wins p board play = any (>= pred winningAmount) 
 -                            [vertical,horizontal,diagonalLeft, diagonalRight]
 -    where march :: (Int -> Int) -> (Int -> Int) -> Position -> Int
 -          march updateX updateY (x,y,p) =
 -              let x' = updateX x
 -                  y' = updateY y
 -                  outOfBounds = x' < 1 || x' > width || y' < 1 || y' > height
 -                  myPlay      = (not outOfBounds) && isJust $ 
 -                                    find (\(a,b,q) -> a == x' && b == y' &&
 -                                                      p == q) board
 -              in if not myPlay then 0 else 1 + march updateX updateY (x',y',p)
 -          vertical      = up + down
 -          horizontal    = left + right
 -          diagonalLeft  = upLeft + upRight
 -          diagonalRight = downLeft + downRight
 -          up        = march id succ play
 -          down      = march id pred play
 -          left      = march pred id play
 -          right     = march succ id play
 -          upLeft    = march pred succ play
 -          upRight   = march succ succ play
 -          downRight = march succ pred play
 -          downLeft  = march pred pred play
 -}

p :: [Move] -> R
p ms = value (movesToBoard ms)

pPar :: [Move] -> [Move] -> R
pPar preceding ms = p (preceding ++ ms)

parEpsilons :: [Move] -> [[Move] -> GL.J R Move]
parEpsilons preceding = take (length poolOfMoves) all'
    where all = epsilonX : epsilonO : all
          all' = drop (length preceding) all
          epsilonX h = GL.argsup (poolOfMoves `GL.setMinus` h)
          epsilonO h = GL.arginf (poolOfMoves `GL.setMinus` h)
          poolOfMoves = sort ((concat $ replicate height [1..width]) 
                            \\ preceding)

-- Find an optimal play prefixed with a set of moves.
parOptimalPlay :: [Move] -> [Move]
parOptimalPlay moves = 
    let result = GL.bigotimes (parEpsilons moves) (pPar moves)
    in  moves ++ result

epsilons :: [[Move] -> GL.J R Move]
epsilons = take (length poolOfMoves) all
    where all  = epsilonX : epsilonO : all
          epsilonX h = GL.argsup (poolOfMoves `GL.setMinus` h)
          epsilonO h = GL.arginf (poolOfMoves `GL.setMinus` h)
          poolOfMoves = sort $ concat $ replicate height [1..width]

-- Uses argmax functions to parallelize the evaluation of optimal moves.
epsilons' :: [[Move] -> GL.J R Move]
epsilons' = take (length poolOfMoves) all'
    where all   = epsilonX : epsilonO : all
          all'  = epsilonX': epsilonO' : all 
          epsilonX h = GL.argsup (poolOfMoves `GL.setMinus` h)
          epsilonO h = GL.arginf (poolOfMoves `GL.setMinus` h)
          epsilonX' h = head . argmax (poolOfMoves `GL.setMinus` h)
          epsilonO' h = head . argmin (poolOfMoves `GL.setMinus` h)
          poolOfMoves = sort $ concat $ replicate height [1..width]

optimalPlay :: [Move]
optimalPlay = GL.bigotimes epsilons' p

optimalOutcome :: R
optimalOutcome = p optimalPlay


-- Determines optimal play by considering possible initial moves in
-- parallel.
parShowOptimalPlay :: IO ()
parShowOptimalPlay = do 
    let allPossibleStarts = (map (:[]) [1..width])
        results = parMap rdeepseq parOptimalPlay allPossibleStarts
        {-results = map parOptimalPlay allPossibleStarts-}
        optimalMoves = GL.argsup results p
        message 
            | p optimalMoves == 1 = "win"
            | p optimalMoves == 0 = "draw"
            | otherwise           = "lose"
    putStrLn $ "For a game of Connect" ++ show winningAmount ++
            " on a " ++ show width ++ "x" ++ show height ++ " grid;"
    putStrLn $ "X " ++ message ++ "s : " ++ show optimalMoves
    putStrLn $ "\n" ++ UL.prettyPrint_ height (movesToBoard optimalMoves) ++ "\n"

-- Determines optimal play sequentially.
showOptimalPlay :: IO ()
showOptimalPlay = print optimalPlay

main = parShowOptimalPlay

-- Generic argmax with parallel map, offered by supervisor (P. Oliva).
argmax :: (NFData a, Ord a) => [a] -> (a -> Int) -> [a]
argmax [] _ = error "argmax : Empty set passed to argmax"
argmax d p = [ x | (v, x) <- graph, v == (fst . last $ graph) ]
    where graph = sort $ parMap rdeepseq (\x -> (p x, x)) d

argmin :: (NFData a, Ord a) => [a] -> (a -> Int) -> [a]
argmin [] _ = error "argmin : Empty set passed to argmin"
argmin d p = argmax d (\x -> - p x)
