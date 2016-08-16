module ConnectN where

import qualified Logic.GameLogic as GL
import Data.List (unionBy, intercalate, sortBy, sort, (\\))
import Data.Maybe (isNothing)
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies
import Control.DeepSeq (force, deepseq, NFData)
import Control.Exception (evaluate)

type R = Int
type Move = Int  
type Coordinate = Int
type Position = (Coordinate, Coordinate, Player)
type Board    = [Position]

width, height, winningAmount :: Int
height =        3
width  =        3
winningAmount = 3

data Player = X | O deriving (Eq, Read, Show, Ord)

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
-- player has matched 3 in a row.
wins :: Player -> Board -> Bool
wins p b = or v || or h || or d
    -- Very verbose and clumsy check for a match of 3 in a row vertically,
    -- horizontally, and diagonally.
    where v, h :: [Bool]
          winningPlay   = replicate winningAmount p
          -- Get all this player's moves.
          actualPlays = filter (\(_,_,p') -> p' == p) b
          v = do  
              -- Check each column for a vertical match of 3 in a row. 
              x <- [1..width]
              let section = map (\(_,_,p') -> p') $ 
                    filter (\(x',_,_) -> x' == x) b
              return $ GL.contained winningPlay section
          h = do
              -- Check each row for a horizontal match of 3 in a row.
              y <- [1..height]
              let section = filter (\(_,y',_) -> y' == y) actualPlays
              return $ length section == winningAmount
              -- Combine the result of checking the left and right
              -- for diagonal matches of 3 in a row.
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
pPar preceding ms = value (movesToBoard $ preceding ++ ms)

parEpsilons :: [Move] -> [[Move] -> GL.J R Move]
parEpsilons preceding = take (length poolOfMoves) all'
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
epsilons = take (length poolOfMoves) all
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
    let rows :: [[(Coordinate, Coordinate, Maybe Player)]]
        rows = do 
                y <- [1..height]
                -- A list of empty spaces we'll combine with the filled ones
                let emptySpaces :: [(Coordinate, Coordinate, Maybe a)]
                    emptySpaces = zip3 [1..width] (repeat y) (repeat Nothing)
                return $ sortBy order $ flip (unionBy isBlank) emptySpaces $ 
                    map (\(a,b,c) -> (a,b,Just c)) $
                    filter (\(_,y',_) -> y' == y) b
        -- If second tuple contains Nothing, return True.
        isBlank :: (Coordinate, Coordinate, Maybe Player) ->
                    (Coordinate, Coordinate, Maybe Player) -> Bool
        isBlank (x,y,m) (x',y',m') = x == x' && y == y' && isNothing m'
        -- Organize rows by their x coordinate.
        order :: (Coordinate, Coordinate, Maybe Player) ->
                  (Coordinate, Coordinate, Maybe Player) -> Ordering
        order (x,_,_) (x',_,_) = compare x x'
        line :: String
        line = "\n" ++ concat ( replicate width " ―――") ++ "\n"
    in 
        intercalate line $ map ( (\s -> "  " ++ s) . intercalate " | " . 
            map (\(_,_,p) -> maybe " " show p)) (reverse rows)

main :: IO ()
main = do 
          --putStrLn $ "\n" ++ (prettyPrint $ movesToBoard optimalPlay) ++ "\n"
         let allPossibleStarts = (map (:[]) [1..width])
             {-results = parMap rdeepseq parOptimalPlay allPossibleStarts-}
             results = map parOptimalPlay allPossibleStarts
             optimalMoves = GL.argsup results p
             message 
                | p optimalMoves == 1 = "win"
                | p optimalMoves == 0 = "draw"
                | otherwise           = "lose"
         putStrLn $ "For a game of Connect" ++ show winningAmount ++
                  " on a " ++ show width ++ "x" ++ show height ++ " grid;"
         putStrLn $ "X " ++ message ++ "s : " ++ show optimalMoves
            where xwins ms = p ms == 1 
