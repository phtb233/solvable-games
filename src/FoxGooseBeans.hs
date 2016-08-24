module FoxGooseBeans where

import qualified Logic.GameLogic as GL
import Data.Tuple (swap)
import Data.List (sort)

type R        = Bool
-- (On the way to right, coming back to left)
-- e.g (Goose, Nothing) to put goose on the right and bring nothing to the left
type Move     = (Maybe Goods, Maybe Goods)
type Side     = (Maybe Goods, Maybe Goods, Maybe Goods)
type Banks    = (Side, Side)

data Goods = Fox | Goose | Beans deriving (Show, Eq, Ord)

-- One good can be moved to the right bank, and another good can be brought
-- back to the left during each turn. To win, the player must move all
-- goods from the left bank to the right. However the Fox cannot be left
-- with the Goose, nor the Goose with the Beans otherwise the player loses.

-- Move goods as requested. If not possible then do nothing.
transferGoods:: Maybe Goods -> Banks -> Banks
transferGoods Nothing b = b          
transferGoods good b@(thisSide, thatSide) 
    | good `isOn` thisSide = (removeGood good thisSide, addGood good thatSide)
    | otherwise            = b
{-

 Fox   |    | _
 Goose |    | _
 Beans |    | _

-}

-- Only present moves that change the banks, omit those that don't.
-- In cases where the length of the game is longer than the optimal
-- solution length, the computation will pad out the solution with
-- idempotent moves (e.g. moving nothing from either bank).
shortenOutcome :: [Move] -> [Move]
shortenOutcome moves = shorten moves ((Just Fox, Just Goose, Just Beans), 
                                        (Nothing, Nothing, Nothing))
    where shorten []     b       = []
          shorten (m:ms) b@(l,r) = 
            let (lm, rm)  = m
                moveRight = transferGoods lm (l,r)
                b'        = swap $ transferGoods rm $ swap moveRight
            in  if b == b' then shorten ms b else m : shorten ms b'

-- Convert list of Moves to the resultant Banks from performing said moves.
movesToBanks :: [Move] -> Banks
movesToBanks moves = mtb moves ((Just Fox, Just Goose, Just Beans), 
                                (Nothing, Nothing, Nothing))
    where mtb :: [Move] -> Banks -> Banks
          mtb []     b     = b
          mtb (m:ms) (l,r) = 
            let (lm, rm) = m
                moveRight = transferGoods lm (l,r)
                moveLeft = swap $ transferGoods rm $ swap moveRight
            in  if wins moveRight || lose (fst moveRight)
                    then moveRight
                    else if wins moveLeft || lose (snd moveLeft)
                             then moveLeft
                             else mtb ms moveLeft
                        
-- Check if a Good exists on a Side.
isOn :: Maybe Goods -> Side -> Bool
isOn g (a,b,c) = g == a || g == b || g == c

wins :: Banks -> Bool
wins ((Nothing, Nothing, Nothing),
      (Just Fox, Just Goose, Just Beans)) = True
wins _ = False

lose :: Side -> Bool
lose (Just Fox, Just Goose, _)   = True
lose (_, Just Goose, Just Beans) = True
lose _                           = False

removeGood, addGood :: Maybe Goods -> Side -> Side
removeGood (Just Fox)   (a,b,c) = (Nothing, b, c)
removeGood (Just Goose) (a,b,c) = (a, Nothing, c)
removeGood (Just Beans) (a,b,c) = (a, b, Nothing)
removeGood _            s       = s
addGood    (Just Fox)   (a,b,c) = (Just Fox, b, c)
addGood    (Just Goose) (a,b,c) = (a, Just Goose, c)
addGood    (Just Beans) (a,b,c) = (a, b, Just Beans)
addGood    _            s       = s

p :: [Move] -> R
p ms = wins $ movesToBanks ms

-- 4 is the least amount of turns it takes to win.
epsilons :: [[Move] -> GL.J R Move]
epsilons = replicate 10 epsilons'
    where epsilons' _ = GL.find poolOfMoves
          poolOfMoves = sort [(x, y) | x <- goods, y <- goods, x /= y]
          goods       = [Just Fox, Just Goose, Just Beans, Nothing]

optimalPlay :: [Move]
optimalPlay = GL.bigotimes epsilons p

main :: IO ()
main = do putStrLn "\nOptimal play : "
          mapM_ (putStrLn . show) optimalPlay
          putStrLn "\nShortened play : "
          mapM_ (putStrLn . show) (shortenOutcome optimalPlay)
