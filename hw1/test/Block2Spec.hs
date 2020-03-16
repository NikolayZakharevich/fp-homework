{-# LANGUAGE InstanceSigs #-}

module Block2Spec where

import Block2
import Data.List.NonEmpty
import Test.Hspec
import TestUtil

spec :: Spec
spec =
  describe "Block 2. Task 2" $ do
    testSplitOn1
    testSplitOn2
    testSplitOn3
    testJoinWith1
    testJoinWith2
    testJoinWith3
    testJoinWith4
    testIdempotenceProperty

testSplitOn1 :: SpecWith (Arg Expectation)
testSplitOn1 =
  uTest
    "splits file path"
    (splitOn '/' "path/to/file")
    ("path" :| ["to", "file"])

testSplitOn2 :: SpecWith (Arg Expectation)
testSplitOn2 =
  uTest
    "splits text that contains only delimiters"
    (splitOn '/' "///")
    ("/" :| ["/"])

testSplitOn3 :: SpecWith (Arg Expectation)
testSplitOn3 =
  uTest
    "splits text without delimiters"
    (splitOn '0' "abacaba")
    ("abacaba" :| [])

testJoinWith1 :: SpecWith (Arg Expectation)
testJoinWith1 =
  uTest
    "joins file path"
    (joinWith '/' ("path" :| ["to", "file"]))
    "path/to/file"

testJoinWith2 :: SpecWith (Arg Expectation)
testJoinWith2 =
  uTest
    "joins parts that contain only delimiters"
    (joinWith '/' ("//" :| ["/", "///"]))
    "////////"

testJoinWith3 :: SpecWith (Arg Expectation)
testJoinWith3 = uTest "joins one part" (joinWith '/' ("path" :| [])) "path"

testJoinWith4 :: SpecWith (Arg Expectation)
testJoinWith4 = uTest "joins empty string" (joinWith '/' ("" :| [])) ""

inverseProperty :: Char -> String -> Bool
inverseProperty delimiter str =
  (joinWith delimiter . splitOn delimiter) str == str

testIdempotenceProperty :: Spec
testIdempotenceProperty = pTest "holds idempotence property" inverseProperty
