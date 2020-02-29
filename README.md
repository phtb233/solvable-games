# Solvable Games

## How to run:

[Stack](https://docs.haskellstack.org/en/stable/README/) is recommended for building and executing this code. 
```bash
$ git clone https://github.com/phtb233/solvable-games
$ cd solvable-games
$ stack run
```

This project features some puzzles solved by the Selection Monad.

Solved in parallel:
  * Sudoku
  * Connect 3 / Connect 4
  * TicTacToe
  * NQueens

Computed sequentially:
  * Futoshiki
  * Nonograms
  * Buchstabensalat
  * Inshi no Heya
  * Nim (SubtractionGame)
  * River Crossing Problem

The selection monad itself is contained in the module 'GameLogic'. 

