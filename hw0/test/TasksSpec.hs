module TasksSpec where

import Task1
import Task2
import Task3
import Task4
import Task5
import Task6
import Test.Hspec

import Data.Char
import Data.List
import Data.Maybe (mapMaybe)

type ESPair = (Either String String, Either String String) -- Either String Pair

spec :: Spec
spec =
  describe "Homework 0" $ do
    testDistributivity1
    testDistributivity2
    testIterating
    testFactorial1
    testFactorial2
    testFibonacci1
    testFibonacci2
    testFibonacci3
    testFibonacci4
    testFibonacci5
    testMap1
    testMap2
    testMap3
    testChurch1
    testChurch2
    testChurch3
    testChurch4
    testWhnf1
    testWhnf2

test ::
     (HasCallStack, Show a, Eq a)
  => String
  -> a
  -> a
  -> SpecWith (Arg Expectation)
test message expression expected = it message $ expression `shouldBe` expected

testDistributivity1 :: SpecWith (Arg Expectation)
testDistributivity1 =
  test
    "Distributivity: 'Left' constructor"
    (distributivity (Left 4) :: (Either Int Int, Either Int Int))
    (Left 4, Left 4)

testDistributivity2 :: SpecWith (Arg Expectation)
testDistributivity2 =
  test
    "Distributivity: 'Right' constructor"
    (distributivity (Right ("a", [4])) :: (Either Int String, Either Int [Int]))
    (Right "a", Right [4])

testIterating :: SpecWith (Arg Expectation)
testIterating =
  test
    "Iterating on list of ones"
    (take 1000 (iterateElement 1))
    (take 1000 [1,1 ..])

testFactorial1 :: SpecWith (Arg Expectation)
testFactorial1 = test "Factorial: 6! == 720" (factorial 6) 720

testFactorial2 :: SpecWith (Arg Expectation)
testFactorial2 = test "Factorial: 0! == 1" (factorial 0) 1

testFibonacci1 :: SpecWith (Arg Expectation)
testFibonacci1 = test "Fibonacci: F(0) == 0" (fibonacci 0) 0

testFibonacci2 :: SpecWith (Arg Expectation)
testFibonacci2 = test "Fibonacci: F(2) == 1" (fibonacci 2) 1

testFibonacci3 :: SpecWith (Arg Expectation)
testFibonacci3 = test "Fibonacci: F(10) == 55" (fibonacci 10) 55

testFibonacci4 :: SpecWith (Arg Expectation)
testFibonacci4 = test "Fibonacci: F(-5) == 5" (fibonacci (-5)) 5

testFibonacci5 :: SpecWith (Arg Expectation)
testFibonacci5 = test "Fibonacci: F(6) == -8" (fibonacci (-6)) (-8)

testMap1 :: SpecWith (Arg Expectation)
testMap1 = test "Map: map (*1) [] == []" (mapFix (* 1) []) []

testMap2 :: SpecWith (Arg Expectation)
testMap2 =
  test "Map: map (*2) [1, 2, 3] == [2, 4, 6]" (mapFix (* 2) [1, 2, 3]) [2, 4, 6]

testMap3 :: SpecWith (Arg Expectation)
testMap3 =
  test "Map: map toUpper \"test\" == \"TEST\"" (mapFix toUpper "test") "TEST"

testChurch1 :: SpecWith (Arg Expectation)
testChurch1 = test "Church zero" (churchToInt zero) 0

testChurch2 :: SpecWith (Arg Expectation)
testChurch2 =
  test
    "Church increrement"
    (churchToInt (succChurch five))
    (1 + churchToInt five)

testChurch3 :: SpecWith (Arg Expectation)
testChurch3 =
  test
    "Church sum"
    (churchToInt (churchPlus three five))
    (churchToInt three + churchToInt five)

testChurch4 :: SpecWith (Arg Expectation)
testChurch4 =
  test
    "Church mul"
    (churchToInt (churchMult three five))
    (churchToInt three * churchToInt five)

testWhnf1 :: SpecWith (Arg Expectation)
testWhnf1 =
  test
    "WHNF: distributivity"
    (distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain")) :: ESPair)
    ( Left ("harold" ++ " hide " ++ "the " ++ "pain")
    , Left ("harold" ++ " hide " ++ "the " ++ "pain"))

testWhnf2 :: SpecWith (Arg Expectation)
testWhnf2 =
  test
    "WHNF: mapMaybe"
    (null $ mapMaybe foo "pole chudes ochen' chudesno")
    False
