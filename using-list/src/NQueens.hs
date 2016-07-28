module NQueens where
import qualified GameLogic as GL
import Data.List ((\\))
import System.Environment
import Control.Parallel.Strategies (parMap, rdeepseq)

type R = Bool
type Coordinate = Int
type Move = Coordinate
type Position = (Coordinate, Coordinate)

size :: Int
size = 8

attacks :: Position -> Position -> Bool
attacks (x, y) (a, b) =
    x == a || y == b || abs(x - a) == abs(y - b)

valid :: [Position] -> Bool
valid [] = True
valid (u : vs) =
    not(any (\v -> attacks u v) vs) && valid vs

p :: [Move] -> R
p ms = valid(zip ms [0..(size-1)])

pPar :: [Move] -> [Move] -> R
pPar preceding = p . (preceding ++)

parOptimalStrategy :: [Move] -> [Move]
parOptimalStrategy preceding = 
         GL.bigotimes (parEpsilons preceding) (pPar preceding)

parOptimalPlay :: [Move] -> [Move]
parOptimalPlay preceding = preceding ++ parOptimalStrategy preceding

optimalPlay :: [Move]
optimalPlay  = GL.bigotimes epsilons p

optimalOutcome :: R
optimalOutcome = p optimalPlay

epsilons :: [[Move] -> GL.J R Move]
epsilons = replicate size epsilon
    where epsilon h = GL.find ([0..(size-1)] `GL.setMinus` h)

parEpsilons :: [Move] -> [[Move] -> GL.J R Move]
parEpsilons preceding = replicate size' epsilon
    where epsilon h   = GL.find (poolOfMoves `GL.setMinus` h)
          size'       = (size - (length preceding))
          poolOfMoves = [0..size-1] \\ preceding

parMain :: IO ()
parMain = do
        let allPossibleStarts = map (:[]) [0..size-1]
            results = parMap rdeepseq parOptimalPlay allPossibleStarts
            optimalMoves = filter p results
        if null optimalMoves
            then putStrLn $ "There are no solutions for " ++
                                      show size ++ "-Queens"
            else do putStrLn $ "There's " ++ (show $ length optimalMoves) ++
                              " solutions for " ++ show size ++ "-Queens : "
                    mapM_ (putStrLn . show) optimalMoves

main :: IO ()
main = parMain
