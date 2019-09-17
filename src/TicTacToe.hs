module TicTacToe where
import qualified Logic.GameLogic as GL
import qualified Logic.Utils as UL
import Data.List ((\\))
import Control.Parallel.Strategies (parMap, rdeepseq)

type R = Int
type Move = Int
type Board = ([Move], [Move])

type Player = UL.Player
x' = UL.X
o' = UL.O

value :: Board -> R
value (x, o) | wins x    = 1
             | wins o    = -1
             | otherwise = 0

outcome :: Player -> [Move] -> Board -> Board
outcome whoever [] board = board
outcome UL.X (m : ms) (x, o) =
    if wins o then (x, o)
    else outcome o' ms (GL.insert m x, o)
outcome UL.O (m :ms) (x, o) =
    if wins x then (x, o)
    else outcome x' ms (x, GL.insert m o)
    
wins :: [Move] -> Bool
wins = 
    GL.someContained [[0,1,2],[3,4,5],[6,7,8],
                [0,3,6],[1,4,7],[2,5,8],
                [0,4,8],[2,4,6]]

p :: [Move] -> R
p ms = value (outcome x' ms ([], []))

optimalPlay :: [Move]
optimalPlay = GL.bigotimes GL.epsilons p

optimalOutcome :: R
optimalOutcome = p optimalPlay

pPar :: [Move] -> [Move] -> R
pPar preceding ms = value (outcome x' (preceding ++ ms) ([], []))

parEpsilons :: [Move] -> [[Move] -> GL.J R Move]
parEpsilons preceding = take (9 - (length preceding)) all'
    where all = epsilonX : epsilonO : all
          all' = drop (length preceding) all
          epsilonX h = GL.argsup (poolOfMoves `GL.setMinus` h)
          epsilonO h = GL.arginf (poolOfMoves `GL.setMinus` h)
          poolOfMoves = [0..8] \\ preceding

parOptimalStrategy :: [Move] -> [Move]
parOptimalStrategy moves = GL.bigotimes (parEpsilons moves) (pPar moves)

parOptimalPlay :: [Move] -> [Move]
parOptimalPlay moves = moves ++ (parOptimalStrategy moves)

-- Get the next optimal move for either player X or player O.
nextMove :: [Move] -> Move
nextMove played = 
    let possiblePlays, possibleNextMoves, results :: [[Move]]
        possibleNextMoves = map (:[]) $ [0..8] \\ played
        possiblePlays = map (played ++) possibleNextMoves
        results = parMap rdeepseq parOptimalPlay possiblePlays
        optimalMoves = if even (length played)
                           then xwins results
                           else owins results
        optimalMove  = head $ optimalMoves \\ played
        xwins ms  =  GL.argsup ms p
        owins ms  =  GL.arginf ms p
    in optimalMove

-- Go back and forth between the player and an AI that utilizes optimal 
-- strategies.
playMatch :: IO ()
playMatch = 
    do
    let showBoard :: [Move] -> IO()
        showBoard history = 
            putStrLn $ (UL.prettyPrint_ 3 $ movesToBoard history) ++ "\n"
        showTip = putStrLn $ 
                  "\n==== How to play ====\n" ++
                  "\nSpecify a move between 0 - 8:\n" ++
                  "\n 0 | 1 | 2" ++
                  "\n――― ――― ―――" ++
                  "\n 3 | 4 | 5" ++
                  "\n――― ――― ―――" ++
                  "\n 6 | 7 | 8 \n"

    let getMove :: [Move] -> IO Move
        getMove history = do
            showBoard history
            putStrLn ( "history = " ++ show history)
            putStrLn "What is your next move?"
            input <- fmap reads getLine :: IO [(Int, String)]
            if null input 
                then putStrLn "Invalid input" >> getMove history
                else let move = fst $ head input
                     in if move `elem` history
                           then putStrLn "This move is taken." >> 
                                  getMove history
                           else  if move < 0 || move > 8
                                     then putStrLn "Must be between 0 and 8." >>
                                        getMove history
                                     else return move
    let gameLoop :: [Move] -> Player -> Board -> IO ()
        gameLoop history player board
            | value board == 1 = showBoard history >> 
                                 putStrLn (show board) >>
                                 putStrLn "Player X wins"
            | value board == -1 = showBoard history >> putStrLn "Player O wins"
            | length history == 9 = showBoard history >> putStrLn "Its a draw"
            | otherwise = if player == x' 
                            then do
                                 next <- fmap (:[]) $ getMove history
                                 let board' = outcome x' next board
                                 gameLoop (history ++ next) o' board'
                            else do
                                 let next = (:[]) $ nextMove history
                                 let board' = outcome o' next board
                                 gameLoop (history ++ next) x' board'
    showTip
    gameLoop [] x' ([],[])

-- Show the series of plays/outcome when both players play optimally.
getOptimalPlay :: IO ()
getOptimalPlay = do
        let allPossibleStarts = map (:[]) [0..8]
            results = parMap rdeepseq parOptimalPlay allPossibleStarts
            optimalMoves = GL.argsup results p
            message 
                | p optimalMoves == 1 = "x wins"
                | p optimalMoves == 0 = "a draw"
                | otherwise           = "x loses"
        putStrLn $ "The optimal moves are : " ++ show optimalMoves
        putStrLn $ "The optimal outcome is : " ++ message


movesToBoard :: [Move] -> UL.Board
movesToBoard ms = 
        let plays = cycle [x', o']
            getX :: Move -> Int
            getX m = ( m `mod` 3 ) + 1
            getY :: Move -> Int
            getY m = 3 - (m `div` 3)
            moveToPos move player = (getX move, getY move, player)
            in zipWith moveToPos ms plays

main :: IO ()
main = getOptimalPlay
