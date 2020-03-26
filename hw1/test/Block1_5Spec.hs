module Block1_5Spec where

import Block1_5
import Data.Foldable (toList)
import Data.List (sort)
import Test.Hspec
import TestUtil

spec :: Spec
spec = do
  describe "Block 1. Task 3" $ do
    testBstExists1
    testBstExists2
    testBstExists3
    testBstInsert1
    testBstInsert2
    testBstDelete1
    testBstDelete2
    testBstDelete3
    testBstDelete4
  describe "Block 2. Task 1" $ do
    testSortProperty
    testSortProperty

-- Task 3 (BST):
exampleBst :: Bst Int
exampleBst = fromList [5, 7, 6, 3, 2, 1, 3]

testBstExists1 :: SpecWith (Arg Expectation)
testBstExists1 =
  uTest "checks that single element exists in a tree" (exists exampleBst 2) True

testBstExists2 :: SpecWith (Arg Expectation)
testBstExists2 =
  uTest
    "checks that one of many equal elements exists in a tree"
    (exists exampleBst 3)
    True

testBstExists3 :: SpecWith (Arg Expectation)
testBstExists3 =
  uTest "checks that element not exists in a tree" (exists exampleBst 4) False

testBstInsert1 :: SpecWith (Arg Expectation)
testBstInsert1 =
  uTest
    "inserts new element into a tree"
    (toList (insert exampleBst 4))
    [1, 2, 3, 3, 4, 5, 6, 7]

testBstInsert2 :: SpecWith (Arg Expectation)
testBstInsert2 =
  uTest
    "inserts existing element into a tree"
    (toList (insert exampleBst 2))
    [1, 2, 2, 3, 3, 5, 6, 7]

testBstDelete1 :: SpecWith (Arg Expectation)
testBstDelete1 =
  uTest "deletes a tree leaf" (toList (delete exampleBst 2)) [1, 3, 3, 5, 6, 7]

testBstDelete2 :: SpecWith (Arg Expectation)
testBstDelete2 =
  uTest
    "deletes a tree node with one child"
    (toList (delete exampleBst 1))
    [2, 3, 3, 5, 6, 7]

testBstDelete3 :: SpecWith (Arg Expectation)
testBstDelete3 =
  uTest
    "deletes a tree node with two children"
    (toList (delete exampleBst 6))
    [1, 2, 3, 3, 5, 7]

testBstDelete4 :: SpecWith (Arg Expectation)
testBstDelete4 =
  uTest
    "deletes not existing element from tree"
    (toList (delete exampleBst 4))
    [1, 2, 3, 3, 5, 6, 7]

sortProperty :: [Int] -> Bool
sortProperty xs = (toList . fromList) xs == sort xs

testSortProperty :: Spec
testSortProperty = pTest "holds tree sorting" sortProperty
