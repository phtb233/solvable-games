module Main where

import qualified Logic.GameLogic as GL
-- Solutions for these modules are evaluated
-- in parallel.
import qualified ParSudoku.Sudoku4 as S4
import qualified ParSudoku.Sudoku6 as S6
import qualified ConnectN as CN
import qualified TicTacToe as TTT
import qualified NQueens as NQ
-- But these are not.
import qualified Sudoku as S
import qualified Nonograms as NN
import qualified Buchstabensalat as BS
import qualified Futoshiki as FS
import qualified InshiNoHeya as INH
import qualified FoxGooseBeans as FGB
import qualified NimPencils as NP
import System.Random
import Control.Monad
import Control.Exception

-- To run a program, just specify its main function with a prefix as shown.
-- Then run cabal build && cabal run

main :: IO ()
main = NN.main
