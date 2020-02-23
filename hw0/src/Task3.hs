module Task3
  ( identity
  , contraction
  , composition
  , permutation
  ) where

-- | Always returns the same value that was used as its argument.
identity :: a -> a
identity = s k k

-- | Multiplication in associative unital algebra over a ring.
contraction :: (a -> a -> b) -> a -> b
contraction = s s (k i)

-- | Function composition.
composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (k s) k

-- | Swaps first two arguments.
permutation :: (a -> b -> c) -> b -> a -> c
permutation = s (s (k s) (s (k k) s)) (k k)

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

k :: a -> b -> a
k = const

i :: a -> a
i = identity
