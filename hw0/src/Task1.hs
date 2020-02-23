{-# LANGUAGE TypeOperators #-}

module Task1
  ( distributivity
  , associator
  , eitherAssoc
  ) where

type (<->) a b = (a -> b, b -> a)

-- | Applies distributive law to Either type.
distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left x1)        = (Left x1, Left x1)
distributivity (Right (x2, x3)) = (Right x2, Right x3)

-- | Takes right-associative pair and returns left-associative pair.
associator :: (a, (b, c)) -> ((a, b), c)
associator (x1, (x2, x3)) = ((x1, x2), x3)

-- | Returns pair of functions, both functions change
-- associativity of Either type.
eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (aImplB, bImplA)
  where
    aImplB x =
      case x of
        Left x1          -> Left (Left x1)
        Right (Left x2)  -> Left (Right x2)
        Right (Right x3) -> Right x3
    bImplA x =
      case x of
        Left (Left x1)  -> Left x1
        Left (Right x2) -> Right (Left x2)
        Right x3        -> Right (Right x3)
