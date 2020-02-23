module Task4
  ( iterateElement
  , fibonacci
  , factorial
  , mapFix
  ) where

import Data.Function (fix)

-- | Returns an infinite list of repeated elements.
iterateElement :: a -> [a]
iterateElement x = fix (x :)

-- | Returns nth Fibonacci number
fibonacci :: Integer -> Integer
fibonacci = fix fibonacci'

fibonacci' :: (Integer -> Integer) -> Integer -> Integer
fibonacci' f x
  | x < 0 = f (x + 2) - f (x + 1)
fibonacci' _ 0 = 0
fibonacci' _ 1 = 1
fibonacci' f x = f (x - 1) + f (x - 2)

-- | If argument is >= 0 returns factorial of it, otherwise returns 0.
factorial :: Integer -> Integer
factorial = fix factorial'

factorial' :: (Integer -> Integer) -> Integer -> Integer
factorial' _ 0 = 1
factorial' f x
  | x > 0 = f (x - 1) * x
factorial' _ _ = 0

-- | Returns the list obtained by applying function to each
-- element of list passed as second argument.
mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix mapFix'

mapFix' :: ((a -> b) -> [a] -> [b]) -> (a -> b) -> [a] -> [b]
mapFix' _ _ []          = []
mapFix' _ mapper [x]    = [mapper x]
mapFix' f mapper (x:xs) = mapper x : f mapper xs
