{-# LANGUAGE InstanceSigs #-}

module Block5Spec where

import Block5
import Test.Hspec
import TestUtil

import Control.Monad.State (State, evalState)
import Data.Maybe (fromMaybe)

spec :: Spec
spec =
  describe "Task 2" $ do
    testEval1
    testEval2
    testQueue1
    testQueue2
    testQueue3
    testQueue4
    testMoving1
    testMoving2

testEval1 :: SpecWith (Arg Expectation)
testEval1 =
  uTest "evaluates sum expreession" (eval $ Add (Const 2) (Const 5)) (Right 7)

testEval2 :: SpecWith (Arg Expectation)
testEval2 =
  uTest
    "evaluates sum expreession"
    (eval $ Div (Const 2) (Const 0))
    (Left DivisionByZero)

testQueue ::
     String
  -> State (Queue Double) (Maybe Double)
  -> Double
  -> SpecWith (Arg Expectation)
testQueue message queueOps =
  uTest message (fromMaybe (-1) $ evalState queueOps ([], []))

testQueue1 :: SpecWith (Arg Expectation)
testQueue1 = testQueue "push + pop" (push 4 >> pop) 4

testQueue2 :: SpecWith (Arg Expectation)
testQueue2 = testQueue "pop" pop (-1)

testQueue3 :: SpecWith (Arg Expectation)
testQueue3 = testQueue "pop + pop + push + pop" (pop >> pop >> push 0 >> pop) 0

testQueue4 :: SpecWith (Arg Expectation)
testQueue4 =
  testQueue "push + push + pop + pop" (push 1 >> push 2 >> pop >> pop) 2

testMoving1 :: SpecWith (Arg Expectation)
testMoving1 =
  uTest
    "moving"
    (moving 4 [1, 5, 3, 8, 7, 9, 6])
    [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]

testMoving2 :: SpecWith (Arg Expectation)
testMoving2 =
  uTest
    "moving"
    (moving 2 [1, 5, 3, 8, 7, 9, 6])
    [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
