module NimPencils where

import qualified Logic.GameLogic as GL
import Data.List (sort, (\\))
import Debug.Trace (traceShow)
import Control.Monad
import Control.Parallel.Strategies

-- The player that takes the last set of pencils wins.
-- Conversely, the player that reduces the set of pencils to between 3 and
-- 1 loses.  Each player can take between 1 and 3 pencils per turn.
-- The game starts with <pencilsConstant> pencils.

type R       = Int
type Move    = Int
type Pencils = Int

data Player = X | O deriving (Show, Eq)

pencilsConstant = 21

takeTurn :: Move -> Pencils -> Pencils
takeTurn m pencils = pencils - m

-- Shorten the list of moves to those before the game's conclusion.
shortenOutcome :: [Move] -> [Move]
shortenOutcome moves = shorten moves pencilsConstant
    where shorten [] _ = []
          shorten (m:ms) p = if p > 3 then m : shorten ms (p - m) else []

wins :: Pencils -> Bool
wins p | p <= 0 = error "Out of bounds" 
       | p <= 3 = True 
       | otherwise = False

-- Play the game, returning the winning player and the number of pencils
-- remaining.
outcome :: Player -> [Move] -> Pencils -> (Player, Pencils)
outcome player [] pencils = (player, pencils)
outcome player (m:ms) pencils
    | wins pencils   = (player, pencils)
    | otherwise       = outcome otherPlayer ms pencils'
    where pencils'    = takeTurn m pencils
          otherPlayer = if player == X then O else X

-- Who won, from player X's perspective.
value :: Player -> Pencils -> R
value player pencils
    | player == X && wins pencils = 1
    | player == O && wins pencils = -1
    | otherwise = 0

p :: [Move] -> R
p moves = let (player, pencils) = outcome X moves pencilsConstant
          in   value player pencils 

pPar :: [Move] -> [Move] -> R
pPar preceding ms = p (preceding ++ ms)

parEpsilons :: [Move] -> [[Move] -> GL.J R Move]
parEpsilons preceding = take ((pencilsConstant - 3) - length preceding) all'
    where all = epsilonX : epsilonO : all
          all' = drop (length preceding) all
          epsilonX _ = GL.argsup ([1,2,3])
          epsilonO _ = GL.arginf ([1,2,3])

parOptimalPlay :: [Move] -> [Move]
parOptimalPlay moves = moves ++ parOptimalStrategy moves
    where parOptimalStrategy :: [Move] -> [Move]
          parOptimalStrategy m = GL.bigotimes (parEpsilons m) (pPar m)

-- Find the next optimal move to play.
nextMove :: [Move] -> Move
nextMove played = 
        let possiblePlays = map ((++) played . (:[])) [1..3]
            results = parMap rdeepseq parOptimalPlay possiblePlays
            optimalMoves = if even (length played)
                               then xwins results
                               else owins results
            optimalMove = head $ optimalMoves \\ played
            xwins ms = GL.argsup ms p
            owins ms = GL.arginf ms p
        in optimalMove

-- Play against an AI opponent.
playMatch :: IO ()
playMatch = do
    let showHistory history = putStrLn (flip replicate '|' $ 
                                    max 0 (pencilCount history))
        pencilCount history = pencilsConstant - sum history
        getMove :: [Move] -> IO Move
        getMove history = do
            showHistory history
            putStrLn $ "There's " ++ show (pencilCount history) 
                            ++ " pencils left. What is your next move?"
            input <- fmap reads getLine :: IO [(Int, String)]
            if null input 
                then putStrLn "Invalid input" >> getMove history
                else let (move,_) = head input
                     in if move < 1 || move > 3
                            then putStrLn "Must be within 1 and 3." >>
                                    getMove history
                            else return move
        gameLoop :: [Move] -> Player -> Pencils -> IO ()
        gameLoop history player pencils
            | value player pencils == 1  = showHistory history >> 
                                            putStrLn "Player X wins"
            | value player pencils == -1 = showHistory history >>
                                            putStrLn "Player O wins"
            | length history == pencilsConstant - 3 =
                putStrLn "It's a draw"
            | otherwise = 
                if player == X
                    then do
                        next <- (:[]) <$> getMove history
                        let (_,pencils') = outcome X next pencils
                        gameLoop (history ++ next) O pencils'
                    else do
                        let next = (:[]) $ nextMove history
                        putStrLn $ "The opponent took " ++ (show $ head next)
                        let (_,pencils') = outcome O next pencils
                        gameLoop (history ++ next) X pencils'
    gameLoop [] X pencilsConstant

optimalPlay :: [Move]
optimalPlay = GL.bigotimes epsilons p

epsilons :: [[Move] -> GL.J R Move]
-- Maximum amount of moves considers both players picking 1 every turn.
epsilons = take (pencilsConstant - 3) all
    where all = epsilonX : epsilonO : all
            -- This is not a history dependent game. Ignore h.
          epsilonX _ = GL.argsup ([1,2,3])
          epsilonO _ = GL.arginf ([1,2,3])

getOptimalStuff :: IO ()
getOptimalStuff = 
    do putStrLn $ "The optimal play = " ++ show (shortenOutcome optimalPlay)
       putStrLn $ "The optimalOutcome = " ++ show (p optimalPlay)

-- Start a game loop, terminated by CTRL-C.
gameLoop :: IO ()
gameLoop = forever $ do { playMatch ; putStrLn "\nNew match\n" }

main = gameLoop
