module Main where

import qualified GameLogic as GL
import qualified Sudoku4 as S4
import qualified Sudoku6 as S6
import qualified Sudoku9 as S9
import qualified SudokuN as SN
import qualified ConnectN as CN
import System.Random
import Control.Monad
import Control.Exception

main :: IO ()
main = S4.main
