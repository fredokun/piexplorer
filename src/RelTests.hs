
module RelTests (runTests) where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Rel

import Test.HUnit

-- printing tests

testRel1 = insertRel "a" (1::Int) emptyRel

relTest1 = TestCase $ assertEqual "insertRel1"
           (Map.fromList [("a",Set.fromList [1])]) testRel1

testRel2 = insertRel "a" (2::Int) testRel1

relTest2 = TestCase $ assertEqual "insertRel2"
           (Map.fromList [("a",Set.fromList [1, 2])]) testRel2

testRel3 = insertRel "b" (3::Int) testRel2

relTest3 = TestCase $ assertEqual "insertRel3"
           (Map.fromList [("a",Set.fromList [1, 2]),
                          ("b",Set.fromList [3])]) testRel3

relTests = TestList [TestLabel "relTest1" relTest1,
                     TestLabel "relTest2" relTest2,
                     TestLabel "relTest3" relTest3
                    ]

runTests = runTestTT relTests
