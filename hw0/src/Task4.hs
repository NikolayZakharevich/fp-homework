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
-- For example:
--
-- >>> fibonacci 10
-- 55
--
-- >>> fibonacci -6
-- -8
--
fibonacci :: Integer -> Integer
fibonacci = fix fibonacci'
  where
    fibonacci' :: (Integer -> Integer) -> Integer -> Integer
    fibonacci' f x
      | x > 1 = f (x - 1) + f (x - 2)
      | x < 0 = f (x + 2) - f (x + 1)
      | otherwise = x

-- | If argument is >= 0 returns factorial of it, otherwise returns 0.
-- For example:
--
-- >>> factorial 6
-- 720
--
-- >>> factorial (-2)
-- 0
--
factorial :: Integer -> Integer
factorial = fix factorial'
  where
    factorial' :: (Integer -> Integer) -> Integer -> Integer
    factorial' f x
      | x > 0 = f (x - 1) * x
      | x == 0 = 1
      | otherwise = 0

-- | Returns the list obtained by applying function to each
-- element of list passed as second argument.
-- For example:
--
-- >>> mapFix (* 2) [1, 2, 3]
-- [2, 4, 6]
--
-- >>> mapFix (* 1) []
-- []
--
mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix mapFix'
  where
    mapFix' :: ((a -> b) -> [a] -> [b]) -> (a -> b) -> [a] -> [b]
    mapFix' _ _ []          = []
    mapFix' _ mapper [x]    = [mapper x]
    mapFix' f mapper (x:xs) = mapper x : f mapper xs
