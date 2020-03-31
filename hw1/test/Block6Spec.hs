module Block6Spec where

import Block6
import Test.Hspec
import TestUtil

spec :: Spec
spec = do
  describe "Task 2" $ do
    testElement1
    testElement2
    testStream1
    testStream2
    testStream3
    testStream4
    testStream5
  describe "Task 3" $ do
    testCbs1
    testCbs2
    testCbs3
    testCbs4
    testCbs5
    testCbs6
    testParseInt1
    testParseInt2
    testParseInt3
    testParseInt4
    testParseInt5
    testParseInt6
    testParseInt7
    testParseInt8
    testParseInt9
    testParseInt10
  describe "Task 4" $ do
    testListList1
    testListList2
    testListList3
    testListList4
    testListList5
    testListList6
    testListList7
    testListList8
    testListList9
    testListList10

parserTest ::
     (HasCallStack, Show a, Eq a)
  => Parser Char a
  -> String
  -> String
  -> Maybe (a, String)
  -> SpecWith (Arg Expectation)
parserTest parser message str expected =
  uTest message (runParser parser str) expected

testElement1 :: SpecWith (Arg Expectation)
testElement1 =
  parserTest (element 'a') "parsed valid element" "abc" $ Just ('a', "bc")

testElement2 :: SpecWith (Arg Expectation)
testElement2 =
  parserTest (element 'd') "failed with another element" "abc" Nothing

testStream1 :: SpecWith (Arg Expectation)
testStream1 =
  parserTest (stream "abac") "parsed equal prefix" "abacaba" $
  Just ("abac", "aba")

testStream2 :: SpecWith (Arg Expectation)
testStream2 =
  parserTest (stream "") "parsed empty stream" "abacaba" $ Just ("", "abacaba")

testStream3 :: SpecWith (Arg Expectation)
testStream3 =
  parserTest (stream "abacaba") "parsed equal stream" "abacaba" $
  Just ("abacaba", "")

testStream4 :: SpecWith (Arg Expectation)
testStream4 =
  parserTest (stream "ba") "failed with another prefix" "abacaba" Nothing

testStream5 :: SpecWith (Arg Expectation)
testStream5 = parserTest (stream "bac") "stream" "abacaba" Nothing

testCbs1 :: SpecWith (Arg Expectation)
testCbs1 = parserTest parseCbs "parsed (())()" "(())()" $ Just ((), "")

testCbs2 :: SpecWith (Arg Expectation)
testCbs2 = parserTest parseCbs "parsed ()" "()" $ Just ((), "")

testCbs3 :: SpecWith (Arg Expectation)
testCbs3 = parserTest parseCbs "parsed (())()ab" "(())()ab" $ Just ((), "ab")

testCbs4 :: SpecWith (Arg Expectation)
testCbs4 = parserTest parseCbs "failed at (()ab)" "(()ab)" $ Nothing

testCbs5 :: SpecWith (Arg Expectation)
testCbs5 = parserTest parseCbs "failed at ((((" "((((" $ Nothing

testCbs6 :: SpecWith (Arg Expectation)
testCbs6 = parserTest parseCbs "failed at )()" ")()" $ Nothing

testParseInt1 :: SpecWith (Arg Expectation)
testParseInt1 =
  parserTest parseInt "parsed 999999999" "999999999" $ Just (999999999, "")

testParseInt2 :: SpecWith (Arg Expectation)
testParseInt2 =
  parserTest parseInt "parsed -123456" "-123456" $ Just (-123456, "")

testParseInt3 :: SpecWith (Arg Expectation)
testParseInt3 =
  parserTest parseInt "parsed +123456" "+123456" $ Just (123456, "")

testParseInt4 :: SpecWith (Arg Expectation)
testParseInt4 =
  parserTest parseInt "parsed 00000" "00000" $ Just (0, "0000")

testParseInt5 :: SpecWith (Arg Expectation)
testParseInt5 = parserTest parseInt "parsed 0" "0" $ Just (0, "")

testParseInt6 :: SpecWith (Arg Expectation)
testParseInt6 =
  parserTest parseInt "parsed -01234" "-01234" $ Just (0, "1234")

testParseInt7 :: SpecWith (Arg Expectation)
testParseInt7 =
  parserTest parseInt "parsed +123a123" "+123a123" $ Just (123, "a123")

testParseInt8 :: SpecWith (Arg Expectation)
testParseInt8 = parserTest parseInt "failed at --1" "--1" $ Nothing

testParseInt9 :: SpecWith (Arg Expectation)
testParseInt9 = parserTest parseInt "failed at +-0" "+-0" $ Nothing

testParseInt10 :: SpecWith (Arg Expectation)
testParseInt10 = parserTest parseInt "failed at a123" "a123" $ Nothing

testListList1 :: SpecWith (Arg Expectation)
testListList1 =
  parserTest listlistParser "parsed two lists" "2, 1,+10  , 3,5,-7, 2" $
  Just ([[1, 10], [5, -7, 2]], "")

testListList2 :: SpecWith (Arg Expectation)
testListList2 = parserTest listlistParser "parsed no lists" "" $ Just ([], "")

testListList3 :: SpecWith (Arg Expectation)
testListList3 =
  parserTest listlistParser "parsed three empty lists" "0, 0, 0" $
  Just ([[], [], []], "")

testListList4 :: SpecWith (Arg Expectation)
testListList4 =
  parserTest listlistParser "parsed empty list" "0" $ Just ([[]], "")

testListList5 :: SpecWith (Arg Expectation)
testListList5 =
  parserTest
    listlistParser
    "parsed lists with many spaces"
    "  3, 5   ,-4, 5,0,    1,     2" $
  Just ([[5, -4, 5], [], [2]], "")

testListList6 :: SpecWith (Arg Expectation)
testListList6 =
  parserTest
    listlistParser
    "parsed until no comma"
    "1, 3, 2, +5,  0,    3 ,4 5,0,    1,     2" $
  Just ([[3], [5, 0]], ",    3 ,4 5,0,    1,     2")

testListList7 :: SpecWith (Arg Expectation)
testListList7 =
  parserTest
    listlistParser
    "parsed until invalid number of elements"
    "  3, -5   ,4, 5,0,    1,     2" $
  Just ([[-5, 4, 5], [], [2]], "")

testListList8 :: SpecWith (Arg Expectation)
testListList8 =
  parserTest listlistParser "failed when not enough elements" "5, 2, 3, 5, 0" $
  Just ([], "5, 2, 3, 5, 0")

testListList9 :: SpecWith (Arg Expectation)
testListList9 =
  parserTest
    listlistParser
    "failed when invalid character"
    "  3, 5   a,4, 5,0,    1,     2" $
  Just ([], "  3, 5   a,4, 5,0,    1,     2")

testListList10 :: SpecWith (Arg Expectation)
testListList10 =
  parserTest
    listlistParser
    "failed when invalid length"
    "  -3, 5   ,4, 5,0,    1,     2" $
  Just ([], "  -3, 5   ,4, 5,0,    1,     2")
