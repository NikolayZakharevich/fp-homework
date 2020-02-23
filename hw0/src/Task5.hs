module Task5
  ( churchToInt
  , succChurch
  , churchPlus
  , churchMult
  , zero
  , three
  , five
  ) where

type Nat a = (a -> a) -> a -> a

-- | Increment of church number.
succChurch :: Nat a -> Nat a
succChurch n f x = f (n f x)

-- | Sum of two church numbers.
churchPlus :: Nat a -> Nat a -> Nat a
churchPlus n m f x = n f (m f x)

-- | Multiplication of two church numbers.
churchMult :: Nat a -> Nat a -> Nat a
churchMult n m f = n (m f)

-- | Returns integer representation of church numbers.
churchToInt :: Nat Integer -> Integer
churchToInt n = n (+ 1) 0

-- | Church zero.
zero :: Nat a
zero _ x = x

-- | Church three.
three :: Nat a
three f x = f $ f $ f x

-- | Church five.
five :: Nat a
five f x = f $ f $ f $ f $ f x
