module Block1Spec where

import Block1
import Test.Hspec
import TestUtil

spec :: Spec
spec = do
  describe "Task 1" $ do
    testNextDay1
    testNextDay2
    testNextDay3
    testAfterDays1
    testAfterDays2
    testAfterDays3
    testAfterDays4
    testAfterDays5
    testDaysToParty1
    testDaysToParty2
    testDaysToParty3
  describe "Task 2" $ do
    testNatAdd1
    testNatAdd2
    testNatAdd3
    testNatMul1
    testNatMul2
    testNatMul3
    testNatSub1
    testNatSub2
    testNatSub3
    testNatToInt1
    testNatToInt2
    testNatCompare1
    testNatCompare2
    testNatCompare3
    testNatIsEven1
    testNatIsEven2
    testNatDiv1
    testNatDiv2
    testNatDiv3
    testNatMod1
    testNatMod2
    testNatMod3
    testNatMod4
    testSumCommutativityProperty
    testMulCommutativityProperty
    testSumAssociativityProperty
    testMulAssociativityProperty
    testDistributivityProperty

-- Task 1 (days of week):
testNextDay1 :: SpecWith (Arg Expectation)
testNextDay1 =
  uTest
    "returns the next day for the first day of week"
    (nextDay Monday)
    Tuesday

testNextDay2 :: SpecWith (Arg Expectation)
testNextDay2 =
  uTest "returns the next day for the last day of week" (nextDay Sunday) Monday

testNextDay3 :: SpecWith (Arg Expectation)
testNextDay3 =
  uTest
    "returns the next day for the day from the middle of week"
    (nextDay Tuesday)
    Wednesday

testAfterDays1 :: SpecWith (Arg Expectation)
testAfterDays1 =
  uTest "calculates the day within the same week" (afterDays 4 Monday) Friday

testAfterDays2 :: SpecWith (Arg Expectation)
testAfterDays2 =
  uTest
    "calculates the day from the previous week"
    (afterDays (-2) Monday)
    Saturday

testAfterDays3 :: SpecWith (Arg Expectation)
testAfterDays3 =
  uTest "calculates the day from next week" (afterDays 3 Saturday) Tuesday

testAfterDays4 :: SpecWith (Arg Expectation)
testAfterDays4 = uTest "returns the same day" (afterDays 0 Monday) Monday

testAfterDays5 :: SpecWith (Arg Expectation)
testAfterDays5 =
  uTest "calculates the day from a distant week" (afterDays 23 Monday) Wednesday

testDaysToParty1 :: SpecWith (Arg Expectation)
testDaysToParty1 =
  uTest "calculates days to party on next week " (daysToParty Sunday) 5

testDaysToParty2 :: SpecWith (Arg Expectation)
testDaysToParty2 =
  uTest "calculates days to party on the same week" (daysToParty Thursday) 1

testDaysToParty3 :: SpecWith (Arg Expectation)
testDaysToParty3 =
  uTest "calculates days to today's party" (daysToParty Friday) 0

-- Task 2 (natural numbers):
zero :: Nat
zero = Z

one :: Nat
one = S zero

two :: Nat
two = S one

three :: Nat
three = S two

four :: Nat
four = S three

five :: Nat
five = S four

testNatAdd1 :: SpecWith (Arg Expectation)
testNatAdd1 = uTest "adds a lesser number to a greater number" (two + one) three

testNatAdd2 :: SpecWith (Arg Expectation)
testNatAdd2 = uTest "adds a greater number to a lesser number" (one + two) three

testNatAdd3 :: SpecWith (Arg Expectation)
testNatAdd3 = uTest "adds zero" (three + Z) three

testNatMul1 :: SpecWith (Arg Expectation)
testNatMul1 =
  uTest "multiplies a lesser number by a greater number" (one * two) two

testNatMul2 :: SpecWith (Arg Expectation)
testNatMul2 = uTest "multiplies equal numbers" (two * two) four

testNatMul3 :: SpecWith (Arg Expectation)
testNatMul3 = uTest "multiplies by zero" (one * zero) zero

testNatSub1 :: SpecWith (Arg Expectation)
testNatSub1 =
  uTest "subtracts a smaller number from a larger number" (four - one) three

testNatSub2 :: SpecWith (Arg Expectation)
testNatSub2 =
  uTest "subtracts a greater number from a smaller number" (one - four) zero

testNatSub3 :: SpecWith (Arg Expectation)
testNatSub3 = uTest "subtracts zero" (one - zero) one

testNatToInt1 :: SpecWith (Arg Expectation)
testNatToInt1 = uTest "converts Nat five to 5" (toInt five) 5

testNatToInt2 :: SpecWith (Arg Expectation)
testNatToInt2 = uTest "converts Nat zero to 0" (toInt zero) 0

testNatCompare1 :: SpecWith (Arg Expectation)
testNatCompare1 =
  uTest "compares a lesser number to a greater number" (two < three) True

testNatCompare2 :: SpecWith (Arg Expectation)
testNatCompare2 =
  uTest "compares a greater number to a lesser number" (two >= one) True

testNatCompare3 :: SpecWith (Arg Expectation)
testNatCompare3 = uTest "compares equal zeroes" (zero == zero) True

testNatIsEven1 :: SpecWith (Arg Expectation)
testNatIsEven1 = uTest "checks that two is even" (isEven two) True

testNatIsEven2 :: SpecWith (Arg Expectation)
testNatIsEven2 = uTest "check that three is not even" (isEven three) False

testNatDiv1 :: SpecWith (Arg Expectation)
testNatDiv1 =
  uTest
    "divides numbers with non-zero remainder"
    (five `divNat` two)
    (Right two)

testNatDiv2 :: SpecWith (Arg Expectation)
testNatDiv2 =
  uTest "divides numbers with zero remainder" (four `divNat` two) (Right two)

testNatDiv3 :: SpecWith (Arg Expectation)
testNatDiv3 =
  uTest
    "divides by zero with error"
    (two `divNat` zero)
    (Left "Division by zero")

testNatMod1 :: SpecWith (Arg Expectation)
testNatMod1 =
  uTest
    "returns non-zero remainder in a greater number and lesser number division"
    (five `modNat` three)
    (Right two)

testNatMod2 :: SpecWith (Arg Expectation)
testNatMod2 =
  uTest
    "returns non-zero remainder in a lesser number and greater number division"
    (three `modNat` four)
    (Right three)

testNatMod3 :: SpecWith (Arg Expectation)
testNatMod3 =
  uTest
    "divides by zero with error"
    (one `modNat` zero)
    (Left "Division by zero")

testNatMod4 :: SpecWith (Arg Expectation)
testNatMod4 = uTest "returns zero remainder" (four `modNat` two) (Right zero)

sumCommutativityProperty :: Nat -> Nat -> Bool
sumCommutativityProperty a b = a + b == b + a

mulCommutativityProperty :: Nat -> Nat -> Bool
mulCommutativityProperty a b = a * b == b * a

sumAssociativityProperty :: Nat -> Nat -> Nat -> Bool
sumAssociativityProperty a b c = (a + b) + c == a + (b + c)

mulAssociativityProperty :: Nat -> Nat -> Nat -> Bool
mulAssociativityProperty a b c = (a * b) * c == a * (b * c)

distributivityProperty :: Nat -> Nat -> Nat -> Bool
distributivityProperty a b c = a * (b + c) == a * b + a * c

testSumCommutativityProperty :: Spec
testSumCommutativityProperty =
  pTest "holds sum commutativity" sumCommutativityProperty

testMulCommutativityProperty :: Spec
testMulCommutativityProperty =
  pTest "holds mul commutativity" mulCommutativityProperty

testSumAssociativityProperty :: Spec
testSumAssociativityProperty =
  pTest "holds sum associativity" sumAssociativityProperty

testMulAssociativityProperty :: Spec
testMulAssociativityProperty =
  pTest "holds mul associativity" mulAssociativityProperty

testDistributivityProperty :: Spec
testDistributivityProperty = pTest "holds distributivity" distributivityProperty
