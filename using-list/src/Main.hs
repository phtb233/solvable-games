module Main where

import qualified GameLogic as GL
-- Solutions for these modules are evaluated
-- in parallel.
import qualified Sudoku4 as S4
import qualified Sudoku6 as S6
import qualified Sudoku9 as S9
import qualified ConnectN as CN
import qualified TicTacToe as TTT
import qualified NQueens as NQ
-- But these are not.
import qualified Nonograms as NN
import qualified Buchstabensalat as BS
import qualified Futoshiki as FS
import qualified InshiNoHeya as INH
import System.Random
import Control.Monad
import Control.Exception

main :: IO ()
main = S4.main
