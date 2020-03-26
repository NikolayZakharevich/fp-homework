module Block3Spec where

import Block3
import Data.Monoid (All (..), Any (..), Sum (..))
import Test.Hspec
import TestUtil

spec :: Spec
spec =
  describe "Task 1" $ do
    testMaybeConcat1
    testMaybeConcat2
    testMaybeConcat3
    testEitherConcat1
    testEitherConcat2
    testEitherConcat3
    testNonEmptyConcatAssociativity

testMaybeConcat1 :: SpecWith (Arg Expectation)
testMaybeConcat1 =
  uTest
    "concats Just and Nothing"
    (maybeConcat [Just [1 :: Int, 2, 3], Nothing, Just [4, 5]])
    [1, 2, 3, 4, 5]

testMaybeConcat2 :: SpecWith (Arg Expectation)
testMaybeConcat2 = uTest "concats empty list" (maybeConcat []) ([] :: [String])

testMaybeConcat3 :: SpecWith (Arg Expectation)
testMaybeConcat3 =
  uTest
    "concats Nothings"
    (maybeConcat [Nothing, Nothing, Nothing])
    ([] :: [Int])

testEitherConcat1 :: SpecWith (Arg Expectation)
testEitherConcat1 =
  uTest
    "concats Sum and list"
    (eitherConcat
       [ Left (Sum (3 :: Int))
       , Right [1 :: Int, 2, 3]
       , Left (Sum 5)
       , Right [4, 5]
       ])
    (Sum 8, [1, 2, 3, 4, 5])

testEitherConcat2 :: SpecWith (Arg Expectation)
testEitherConcat2 =
  uTest
    "concats Any and All"
    (eitherConcat [Left (Any False), Left (Any True), Right (All False)])
    (Any True, All False)

testEitherConcat3 :: SpecWith (Arg Expectation)
testEitherConcat3 =
  uTest
    "concats emply list"
    (eitherConcat ([] :: [Either String String]))
    (mempty, mempty)

concatAssociativity :: NonEmpty Int -> NonEmpty Int -> NonEmpty Int -> Bool
concatAssociativity a b c = (a <> b) <> c == a <> (b <> c)

testNonEmptyConcatAssociativity :: Spec
testNonEmptyConcatAssociativity =
  pTest "holds associativity property" concatAssociativity
