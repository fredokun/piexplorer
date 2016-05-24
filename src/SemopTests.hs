
module SemopTests (semopTests) where

import Data.List

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Syntax
import Symbolic
import Semop

import Test.HUnit

import Parser

-- printing tests

testProc1 = case parseProcessExpr "testProc1"  "[new(a) c!a.a!b] || d?(x).x!b" of
  ParseExprError err -> error ("testProc1: unexpected error" ++ (show err))
  ParseExpr p -> p

semopTest1 = TestCase $ assertEqual "semopProc1Init"
           "b=b,c=c,d=d |- (new(a){c!<a>.<a>!b.0} || d?(x).<x>!b.0)" (show (initState testProc1))

proc1Syms = case symStep testProc1 Map.empty  of
  Left (SymbolicError err) -> error ("proc1Syms: unexpected symbolic error: " ++ err)
  Right syms -> syms

semopTest2 = TestCase $ assertEqual "semopProc1Sym1"
           "<intern>--[c!__a__]--> (__a__!b.0 || d?(x).<x>!b.0)" (show ((Set.toList proc1Syms) !! 0))

semopTest3 = TestCase $ assertEqual "semopProc1Sym2"
           "--[c!(__a__)]--> (__a__!b.0 || d?(x).<x>!b.0)" (show ((Set.toList proc1Syms) !! 1))

semopTest4 = TestCase $ assertEqual "semopProc1Sym3"
           "--[d?(x)]--> (new(a){c!<a>.<a>!b.0} || <x>!b.0)" (show ((Set.toList proc1Syms) !! 2))

semopTest5 = TestCase $ assertEqual "semopProc1Sym4"
           "--[c=d,tau]--> (__a__!b.0 || __a__!b.0)" (show ((Set.toList proc1Syms) !! 3))

semopTest6 = TestCase $ assertEqual "semopProc1SymLen"
             4 (Set.size proc1Syms)

proc1Trans = semTrans (initState testProc1) proc1Syms

semopTest7 = TestCase $ assertEqual "semopProc1Trans1"
             "--[tau]--> b=b |- (__a__!b.0 || __a__!b.0)"
             (showTrans ((Set.toList proc1Trans) !! 0))

semopTest8 = TestCase $ assertEqual "semopProc1Trans2"
             "--[d??1]--> b=b,c=c,?1=?1 |- (new(a){c!<a>.<a>!b.0} || ?1!b.0)"
             (showTrans ((Set.toList proc1Trans) !! 1))

semopTest9 = TestCase $ assertEqual "semopProc1Trans3"
             "--[c!(!1)]--> b=b,d=d |- (__a__!b.0 || d?(x).<x>!b.0)"
             (showTrans ((Set.toList proc1Trans) !! 2))


semopTests = TestList [TestLabel "semopTest1" semopTest1
                       , TestLabel "semopTest2" semopTest2
                       , TestLabel "semopTest3" semopTest3
                       , TestLabel "semopTest4" semopTest4
                       , TestLabel "semopTest5" semopTest5
                       , TestLabel "semopTest6" semopTest6
                       , TestLabel "semopTest7" semopTest7
                       , TestLabel "semopTest8" semopTest8
                       , TestLabel "semopTest9" semopTest9
                       --, TestLabel "semopTest10" semopTest10
                      ]

runTests = runTestTT semopTests
