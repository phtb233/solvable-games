module Logic.GameLogic where

import Debug.Trace
-- A function that takes a predicate (which accepts a move and produces
-- a boolean), and returns that move.
type J r x = (x -> r) -> x
type K r x = (x -> r) -> r

{-stub values that are overwritten by the actual game implementation-}
type R = Int
type Move = Int
--              (     e    )    (   p  )
-- overline :: (x -> r) -> x -> (x -> r) -> r
overline :: J r x -> K r x
overline e p = p(e p)

-- find :: [x] -> (x -> Bool) -> x
find :: [x] -> J Bool x
find []     p = undefined
find [x]    p = x
find (x:xs) p = if p x then x else find xs p
-- (x -> r) -> x -> (x -> ([x] -> r) -> [x]) -> ([x] -> r) -> [x]
-- e0 (x -> r) -> x            = epsilon element passed an empty list
-- e1 (x -> ([x] -> r) -> [x]) = takes a move and p and returns a list of moves
-- p  ([x] -> r)
-- result  [x]
otimes :: J r x -> (x -> J r [x]) -> J r [x]
otimes e0 e1 p = a0 : a1
    where a0 = e0(\x0 -> overline(e1 x0)(\x1 -> p(x0:x1)))
          a1 = e1 a0 (\x1 -> p(a0:x1))

-- (e:es) is the list returned by epsilons.
-- 
bigotimes :: [[x] -> J r x] -> J r [x]
bigotimes [] = const []
bigotimes (e:es) =
        e[] `otimes` (\x -> bigotimes [\xs -> d(x:xs) | d<-es])

-- [[Move] -> (Move -> R) -> Move ]
epsilons :: [[Move] -> J R Move]
epsilons = take 9 all
       where all = epsilonX : epsilonO : all
             epsilonX h = argsup ([0..8] `setMinus` h)
             epsilonO h = arginf ([0..8] `setMinus` h)

p :: [Move] -> R
p ms = undefined

optimalStrategy :: [Move] -> Move
optimalStrategy as = head (bigotimes epsilons' p')
    where epsilons'  = drop (length as) epsilons
          p'      xs = p(as ++ xs)

{-Check whether the elements in xs are contained in ys-}
contained :: Ord x => [x] -> [x] -> Bool
contained [] ys = True
contained xs [] = False
contained (us@(x : xs)) (y : ys)
    | x == y    = contained xs ys
    | x > y    = contained us ys
    | otherwise = False

{-See if any of the lists xs in xss are contained in ys-}
someContained :: Ord x => [[x]] -> [x] -> Bool
someContained [] ys  = False
someContained xss [] = False
someContained (xs : xss) ys
    = contained xs ys || someContained xss ys

{-Add an element to 'vs' if it's not in the set. Make sure to maintain the 
set's ordering.-}
insert :: Ord x => x -> [x] -> [x]
insert x [] = [x]
insert x (vs@(y : ys))
    | x == y    = vs
    | x < y     = x : vs
    | otherwise = y : insert x ys

{-Remove an element from the set vs.-}
delete :: Ord x => x -> [x] -> [x]
delete x [] = []
delete x (vs@(y : ys))
    | x == y    = ys
    | x < y     = vs
    | otherwise = y : delete x ys

{-Subtract the elements of ys from xs.-}
setMinus :: Ord x => [x] -> [x] -> [x]
{-setMinus xs [] = xs-}
{-setMinus xs (y : ys) = setMinus (delete y xs) ys-}
setMinus = foldl (flip delete)

--Get the first move that returns 1 when 'p' is called on it. (win)
--Otherwise, return the first move that returns 0. (draw)
--Otherwise, return the last element in the set. (lose)
argsup :: [x] -> J Int x
argsup [] p = trace "Error: an empty list was passed to argsup" undefined
argsup (x:xs) p = f xs x (p x)
    where f xs     a    1 = a
          f []     a    r = a
          f (x:xs) a (-1) = f xs x (p x)
          f     xs a    0 = g xs
           where g [] = a
                 g (x:xs) | p x == 1 = x
                          | otherwise = g xs

arginf :: [x] -> J Int x
arginf xs p = argsup xs (\x -> - p x )
