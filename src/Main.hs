module Main where

import qualified Logic.GameLogic as GL

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
main = NP.main

tttSeq = print TTT.optimalPlay
tttPar = TTT.main
