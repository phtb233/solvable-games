module Main where

import qualified Logic.GameLogic as GL
import qualified Logic.Utils as UL

import qualified Futoshiki as FS
import qualified InshiNoHeya as INH
import qualified Sudoku as S
import qualified ParSudoku.Sudoku6 as S6
import qualified NQueens as NQ

import qualified ConnectN as CN
import qualified ParSudoku.Sudoku4 as S4
import qualified Nonograms as NN
import qualified Buchstabensalat as BS
import qualified FoxGooseBeans as FGB
import qualified NimPencils as NP
import qualified TicTacToe as TTT

main :: IO ()
main = do
        let printHelp = do
                putStrLn $ "What do want to play?"
                putStrLn $ "\tInteractive Games:"
                putStrLn $ "\t\tni - NimPencils"
                putStrLn $ "\t\tt - TicTacToe"
                putStrLn $ "\tNon-interactive:"
                putStrLn $ "\t\tfu - Futoshiki"
                putStrLn $ "\t\tc - Connect N"
                putStrLn $ "\t\ti - InshiNoHeya"
                putStrLn $ "\t\ts - Sudoku"
                putStrLn $ "\t\tnq - NQueens"
                putStrLn $ "\t\tno - Nonograms"
                putStrLn $ "\t\tb - Buchstabensalat"
                putStrLn $ "\t\tfo - Fox, Goose, Beans"
                input <- getLine
                case input of
                    'n': 'i': rest -> putStrLn "\nNimPencils\n" >> NP.main
                    't': rest -> putStrLn "\nTicTacToe\n" >> TTT.playMatch
                    'f': 'u': rest -> putStrLn "\nFutoshiki\n" >> FS.main
                    'c': rest -> putStrLn "\nConnect N\n" >> CN.main
                    'i': rest -> putStrLn "\nInshi No Heya\n" >> INH.main 
                    's': rest -> putStrLn "\nSudoku4\n" >> S4.main
                    'n': 'q': rest -> putStrLn "\nNQueens\n" >> NQ.main
                    'n': 'o': rest -> putStrLn "\nNonograms\n" >> NN.main
                    'b': rest -> putStrLn "\nBuchstabensalat\n" >> BS.main
                    'f': 'o': rest -> putStrLn "\nFox, Goose, Beans\n" >> FGB.main
                    otherwise -> putStrLn "Invalid input\n." >> printHelp
        printHelp
            


tttSeq = print TTT.optimalPlay
tttPar = TTT.main
