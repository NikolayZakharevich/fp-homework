module Block3
  ( maybeConcat
  , eitherConcat
  , NonEmpty(..)
  ) where

import Data.Maybe (fromMaybe)
import Test.Hspec.LeanCheck (Listable (..), cons1, list, tiers)

-- | Concatenates Maybe lists from given list.
--
-- >>> maybeConcat [Just [1,2,3], Nothing, Just [4,5]]
-- [1,2,3,4,5]
--
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat l = fromMaybe [] (mconcat l)

-- | Concatenates lefts and rights from given list
--
-- >>> eitherConcat [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]]
-- (Sum {getSum = 8},[1,2,3,4,5])
--
eitherConcat :: (Monoid m, Monoid n) => [Either m n] -> (m, n)
eitherConcat = foldr concatter (mempty, mempty)
  where
    concatter :: (Monoid m, Monoid n) => Either m n -> (m, n) -> (m, n)
    concatter (Left x) (xs, ys)  = (x <> xs, ys)
    concatter (Right y) (xs, ys) = (xs, y <> ys)

data NonEmpty a =
  a :| [a]
  deriving (Show)

instance Eq a => Eq (NonEmpty a) where
  (x :| []) == (y :| []) = x == y
  (x :| xs) == (y :| ys) = x == y && xs == ys

instance Semigroup (NonEmpty a) where
  (x :| xs) <> (y :| ys) = x :| (xs ++ y : ys)

data ThisOrThat a b
  = This a
  | That b
  | Both a b

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
  This a1 <> This a2 = This (a1 <> a2)
  This a1 <> That b2 = Both a1 b2
  This a1 <> Both a2 b2 = Both (a1 <> a2) b2
  That b1 <> This a2 = Both a2 b1
  That b1 <> That b2 = That (b1 <> b2)
  That b1 <> Both a2 b2 = Both a2 (b1 <> b2)
  Both a1 b1 <> This a2 = Both (a1 <> a2) b1
  Both a1 b1 <> That b2 = Both a1 (b1 <> b2)
  Both a1 b1 <> Both a2 b2 = Both (a1 <> a2) (b1 <> b2)

instance (Integral a, Listable a) => Listable (NonEmpty a) where
  tiers = cons1 toNonEmpty
    where
      toNonEmpty :: (Integral a, Listable a) => a -> NonEmpty a
      toNonEmpty x =
        case list of
          []     -> 0 :| []
          (a : as) -> a :| take (min 10 $ fromIntegral x) as
