module Task2
  ( doubleNeg
  , excludedNeg
  , pierce
  , doubleNegElim
  , thirdNegElim
  ) where

import Data.Void (Void)

type Neg a = a -> Void

-- | Represents double negation introduction.
doubleNeg :: a -> Neg (Neg a)
doubleNeg x f = f x

-- | Represents double negation of exclusion introduction. Can be proven
-- by Glivenko's theorem in intuitionistic logic (because of validity
-- of exclusion introduction in classical logic).
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = ninthAx (contraposition Left) (contraposition Right)

-- | Represents Peirce law. This law does not hold in
-- intuitionistic logic, so the function is undefined.
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- | Represents double negation elimination. It is the 10th
-- axiom of classical logic, but this axiom is changed in
-- intuitionistic logic, so the function is undefined.
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

-- | Represents double negation elimination of negation.
thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim = contraposition doubleNeg

-- | Represents ninth axiom in intuitionistic logic.
ninthAx :: (a -> b) -> (a -> Neg b) -> Neg a
ninthAx aToB aToBtoVoid a = aToBtoVoid a (aToB a)

-- | Represents contraposition.
contraposition :: (a -> b) -> (Neg b -> Neg a)
contraposition aToB bToVoid = bToVoid . aToB
