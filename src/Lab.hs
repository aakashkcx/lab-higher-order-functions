--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lab: Higher-order functions                                                --
--------------------------------------------------------------------------------

module Lab where

--------------------------------------------------------------------------------

-- Some of the functions we will be defining as part of this lab are
-- part of Haskell's standard library. The following line tells the compiler
-- not to import them.
import Prelude hiding ( elem, maximum, intersperse, transpose
                      , subsequences, permutations, any, all, flip, takeWhile
                      , zipWith, groupBy, notElem )

--------------------------------------------------------------------------------
-- Recursive and higher-order functions

elem :: Eq a => a -> [a] -> Bool
-- elem x y = not (filter ((== x)) y == [])
-- elem x = foldr (\a b -> b || a == x) False

maximum :: Ord a => [a] -> a
maximum = foldr1 max

any :: (a -> Bool) -> [a] -> Bool
any p []     = False
any p (x:xs) = p x || any p xs

elem x = any ((==) x)

all :: (a -> Bool) -> [a] -> Bool
all p []     = False
all p [x]    = p x
all p (x:xs) = p x && all p xs

flip :: (a -> b -> c) -> b -> a -> c
flip f a b = f b a

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
    | p x       = x : takeWhile p xs
    | otherwise = []

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f []     _      = []
zipWith f _      []     = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy p []    = []
groupBy p [x]   = []
groupBy p (x:y:xs)
    | p x y     = [x,y] : groupBy p xs
    | otherwise =   [x] : groupBy p (y:xs)

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = undefined

--------------------------------------------------------------------------------
