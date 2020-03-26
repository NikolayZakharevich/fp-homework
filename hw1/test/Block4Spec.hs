module Block4Spec where

import Block4
import Test.Hspec
import TestUtil

spec :: Spec
spec =
  describe "Task 1" $ do
    testStringSum1
    testStringSum2

testStringSum1 :: SpecWith (Arg Expectation)
testStringSum1 = uTest "calculates sum in '1 2 3'" (stringSum "1 2 3") $ Just 6

testStringSum2 :: SpecWith (Arg Expectation)
testStringSum2 =
  uTest "calculates sum in '1 lol 3'" (stringSum "1 lol 3") Nothing
