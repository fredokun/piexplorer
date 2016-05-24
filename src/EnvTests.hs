
module EnvTests (runTests) where

import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Syntax
import Env

import Test.HUnit

-- printing tests

testEnv1 = initEnv [Static "a", Static "b", Static "c"]

envTest1 = TestCase $ assertEqual "envEqualize1"
           "Just Env{a=b,b=b,c=c}"  (show (equalizeNames (Static "a") (Static "b") testEnv1))

envTest2 = TestCase $ assertEqual "envEqualize2"
           "Just Env{a=a,b=b,c=c}"  (show (equalizeNames (Static "a") (Static "a") testEnv1))

envTest3 = TestCase $ assertEqual "envDistinguish1"
           "Just Env{a=a,b=b,c=c,a<>b}" (show (distinguishNames (Static "a") (Static "b") testEnv1))

testEnv2 = fromJust $ equalizeNames (Static "a") (Static "b") testEnv1

envTest4 = TestCase $ assertEqual "envDistinguish2"
           "Nothing" (show (distinguishNames (Static "a") (Static "b") testEnv2))


envTests = TestList [TestLabel "envTest1" envTest1
                    , TestLabel "envTest2" envTest2
                    , TestLabel "envTest3" envTest3
                    , TestLabel "envTest4" envTest4
                    ]

runTests = runTestTT envTests
