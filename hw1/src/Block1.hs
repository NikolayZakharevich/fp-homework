module Block1
  ( DayOfWeek(..)
  , nextDay
  , afterDays
  , isWeekend
  , daysToParty
  , Nat(S, Z)
  , add
  , mul
  , sub
  , lt
  , gte
  , eq
  , toInt
  , isEven
  , divNat
  , modNat
  ) where

import Test.Hspec.LeanCheck (Listable, cons1, tiers)

-- Task 1 (days of week):
data DayOfWeek
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

instance Eq DayOfWeek where
  Monday == Monday = True
  Tuesday == Tuesday = True
  Wednesday == Wednesday = True
  Thursday == Thursday = True
  Friday == Friday = True
  Saturday == Saturday = True
  Sunday == Sunday = True
  _ == _ = False

-- | Returns next day of week.
-- For example:
--
-- >>> nextDay Monday
-- Tuesday
--
-- >>> nextDay Sunday
-- Monday
--
nextDay :: DayOfWeek -> DayOfWeek
nextDay day =
  case day of
    Monday    -> Tuesday
    Tuesday   -> Wednesday
    Wednesday -> Thursday
    Thursday  -> Friday
    Friday    -> Saturday
    Saturday  -> Sunday
    Sunday    -> Monday

-- | If n >= 0 then `afterDays n day` returns the day that is 'n' days
-- after a 'day'. For example:
--
-- >>> afterDays 4 Monday
-- Friday
--
-- >>> afterDays -2 Monday
-- Saturday
--
-- >>> afterDays 3 Saturday
-- Tuesday
--
-- >>> afterDays 0 Monday
-- Monday
--
afterDays :: Int -> DayOfWeek -> DayOfWeek
afterDays n day = iterate dayChanger day !! abs n
  where
    dayChanger :: DayOfWeek -> DayOfWeek
    dayChanger =
      if n < 0
        then prevDay
        else nextDay
    prevDay :: DayOfWeek -> DayOfWeek
    prevDay d =
      case d of
        Monday    -> Sunday
        Tuesday   -> Monday
        Wednesday -> Tuesday
        Thursday  -> Wednesday
        Friday    -> Thursday
        Saturday  -> Friday
        Sunday    -> Saturday

-- | Determines if day of week is weekend
-- For example:
--
-- >>> isWeekend Monday
-- False
--
-- >>> isWeekend Sunday
-- True
--
isWeekend :: DayOfWeek -> Bool
isWeekend day =
  case day of
    Saturday -> True
    Sunday   -> True
    _        -> False

-- | Takes day of week and calculates number of days before Friday.
-- For example:
--
-- >>> daysToParty Sunday
-- 5
--
-- >>> daysToParty Thursday
-- 1
--
daysToParty :: DayOfWeek -> Int
daysToParty Friday = 0
daysToParty day    = daysToParty (nextDay day) + 1

-- Task 2 (natural numbers):
data Nat
  = Z
  | S Nat
  deriving (Show)

-- | Sum of two natural numbers. Implementation based on equations:
-- a + (b + 1) = (a + 1) + b
-- a + 0 = a
add :: Nat -> Nat -> Nat
a `add` Z = a
a `add` (S b) = S a `add` b

-- | Product of two natural numbers. Implementation based on equations:
-- a * (b + 1) = (a * b) + a
-- a + 0 = a
mul :: Nat -> Nat -> Nat
_ `mul` Z = Z
a `mul` (S b) = (a `mul` b) `add` a

-- | Difference of two natural numbers. If 'a' less than 'b' then returns 0.
sub :: Nat -> Nat -> Nat
Z `sub` _ = Z
a `sub` Z = a
(S a) `sub` (S b) = a `sub` b

-- | Check if one natural number is less that the other one.
lt :: Nat -> Nat -> Bool
lt Z (S _)     = True
lt (S a) (S b) = lt a b
lt _ _         = False

-- | Check if two natural numbers are equal.
eq :: Nat -> Nat -> Bool
eq Z Z         = True
eq (S a) (S b) = eq a b
eq _ _         = False

-- | Check if one natural number is greater than or equal to the other one.
gte :: Nat -> Nat -> Bool
gte a b = not (lt a b)

-- | Nat to Int conversion.
toInt :: Nat -> Int
toInt Z     = 0
toInt (S x) = toInt x + 1

-- | Checks if natural number is even.
isEven :: Nat -> Bool
isEven Z     = True
isEven (S x) = not (isEven x)

-- | Quotient of two natural numbers.
divNat :: Nat -> Nat -> Either String Nat
_ `divNat` Z = Left "Division by zero"
a `divNat` b
  | a `gte` b = (S Z `add`) <$> ((a `sub` b) `divNat` b)
  | otherwise = Right Z

-- | Remainder of two natural numbers:
-- a % b = a - b * (a / b)
modNat :: Nat -> Nat -> Either String Nat
a `modNat` b = (a `sub`) . (b `mul`) <$> (a `divNat` b)

-- Two instances, necessary for tests
instance Listable Nat where
  tiers = cons1 intToNat
    where
      intToNat :: Int -> Nat
      intToNat a
        | a <= 0 = Z
        | otherwise = S (intToNat (a - 1))

instance Eq Nat where
  (==) = eq
