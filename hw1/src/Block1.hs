module Block1
  ( DayOfWeek(..)
  , nextDay
  , afterDays
  , isWeekend
  , daysToParty
  , Nat(..)
  , toInt
  , isEven
  , divNat
  , modNat
  ) where

import Test.Hspec.LeanCheck (Listable, cons0, tiers, (\/))

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

-- | Equality check for natural numbers.
instance Eq Nat where
  Z == Z = True
  (S a) == (S b) = a == b
  _ == _ = False

-- | Comparing natural numbers.
instance Ord Nat where
  compare Z Z         = EQ
  compare (S _) Z     = GT
  compare Z (S _)     = LT
  compare (S a) (S b) = compare a b

instance Num Nat where
  abs = undefined
  signum = undefined
  fromInteger = undefined
  -- | Sum of two natural numbers. Implementation based on equations:
  -- a + (b + 1) = (a + 1) + b
  -- a + 0 = a
  a + Z = a
  a + (S b) = S a + b
  -- | Product of two natural numbers. Implementation based on equations:
  -- a * (b + 1) = (a * b) + a
  -- a + 0 = a
  _ * Z = Z
  a * (S b) = (a * b) + a
  -- | Difference of two natural numbers. If 'a' less than 'b' then returns 0.
  Z - _ = Z
  a - Z = a
  (S a) - (S b) = a - b

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
  | a >= b = (S Z +) <$> ((a - b) `divNat` b)
  | otherwise = Right Z

-- | Remainder of two natural numbers:
-- a % b = a - b * (a / b)
modNat :: Nat -> Nat -> Either String Nat
a `modNat` b = (a -) . (b *) <$> (a `divNat` b)

instance Listable Nat where
  tiers = kek Z 0
    where
      kek :: Nat -> Int -> [[Nat]]
      kek a 100     = cons0 a
      kek a counter = cons0 a \/ kek (S a) (counter + 1)
