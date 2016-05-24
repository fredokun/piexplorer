
module DistTests (distTests) where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Syntax

import Dist

import Test.HUnit

-- printing tests

distTest1 = TestCase $ assertEqual "distDistinct1"
           False (distinct (Static "a") (Static "b") emptyDist)

testDist1 = distinguish (Static "a") (Static "b") emptyDist

distTest2 = TestCase $ assertEqual "distDistinct2"
           True (distinct (Static "a") (Static "b") testDist1)

distTest3 = TestCase $ assertEqual "distDistinct3"
           True (distinct (Static "b") (Static "a") testDist1)

testDist2 = distinguish (Static "b") (Static "c") testDist1

distTest4 = TestCase $ assertEqual "distDistinct4"
           "Dist{b<>c}" (show (removeName (Static "a") testDist2))

distTest5 = TestCase $ assertEqual "distDistinct5"
           "Dist{b<>c,b<>d}" (show (replaceName (Static "a") (Static "d") testDist2))


distTests = TestList [TestLabel "distTest1" distTest1
                     , TestLabel "distTest2" distTest2
                     , TestLabel "distTest3" distTest3
                     , TestLabel "distTest4" distTest4
                     , TestLabel "distTest5" distTest5
                    ]

runTests = runTestTT distTests
