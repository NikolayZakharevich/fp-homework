module Block4
  ( stringSum
  ) where

import Text.Read (readMaybe)

-- | Splits string on spaces, converts each part to Int and returns sum of this
-- numbers. If at least one of parts is not Int returns Nothing
stringSum :: String -> Maybe Int
stringSum str = sum <$> traverse readMaybe (words str)

data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a

instance Functor Tree where
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)
  fmap f (Leaf x)     = Leaf (f x)

instance Applicative Tree where
  pure = Leaf
  (Branch lf rf) <*> b = Branch (lf <*> b) (rf <*> b)
  (Leaf f) <*> b = fmap f b

instance Foldable Tree where
  foldr f z tree =
    case tree of
      Branch l r -> foldr f (foldr f z r) l
      Leaf x     -> f x z

instance Traversable Tree where
  traverse f (Branch a b) = Branch <$> traverse f a <*> traverse f b
  traverse f (Leaf x)     = Leaf <$> f x

data NonEmpty a =
  a :| [a]

-- | Converts NonEmpty to List
toList :: NonEmpty a -> [a]
toList (x :| xs) = x : xs

instance Functor NonEmpty where
  fmap f (x :| xs) = f x :| (f <$> xs)

instance Applicative NonEmpty where
  pure x = x :| []
  f :| fs <*> x :| xs = f x :| ((f <$> xs) ++ (fs <*> x : xs))

instance Foldable NonEmpty where
  foldr f z (x :| xs) = f x (foldr f z xs)

instance Traversable NonEmpty where
  traverse f (x :| xs) = (:|) <$> f x <*> traverse f xs

instance Monad NonEmpty where
  x :| xs >>= f = let (y :| ys) = f x in y :| foldr ((++) . toList . f) ys xs
