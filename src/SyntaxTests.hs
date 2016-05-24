
module SyntaxTests (syntaxTests) where

import Syntax

import Test.HUnit

-- printing tests

ppTest1 = TestCase $ assertEqual "(ppProcess Term)"
          "0" (ppProcess (Term undefined))

ppTest2 = TestCase $ assertEqual "(ppProcess (Par Term Term))"
          "(0 || 0)" (ppProcess (Par (Term undefined) (Term undefined) undefined))

ppTest3 = TestCase $ assertEqual "(ppProcess (Sum Term Term))"
          "(0 + 0)" (ppProcess (Sum (Term undefined) (Term undefined) undefined))

ppTest4 = TestCase $ assertEqual "(ppProcess (Tau Term))"
          "tau.0" (ppProcess (Step (Term undefined) undefined))

ppTest5 = TestCase $ assertEqual "(ppProcess (Output (Static \"a\") (Static \"b\") Term))"
          "a!b.0" (ppProcess (Output (Static "a") (Static "b") (Term undefined) undefined))

ppTest6 = TestCase $ assertEqual "(ppProcess (Input (Static \"a\") \"x\" Term))"
          "a?(x).0" (ppProcess (Input (Static "a") "x" (Term undefined) undefined))

ppTest7 = TestCase $ assertEqual "(ppProcess (Restrict \"a\" Term))"
          "new(a){0}" (ppProcess (Restrict "a" (Term undefined) undefined))

ppTest8 = TestCase $ assertEqual "(ppProcess (Call \"K\" ...))"
          "K(v1, v2, v3)"  (ppProcess (Call "K" [Static "v1", Static "v2", Static "v3"] undefined))

ppTest9 = TestCase $ assertEqual "(ppDef (Def \"K\" ...))"
          "K(x1, x2, x3) = <x1>!<x2>.<x3>?(y).0"
          (ppDef (Def "K" ["x1", "x2", "x3"] (Output (PlaceHolder "x1") (PlaceHolder "x2") (Input (PlaceHolder "x3") "y" (Term undefined) undefined) undefined) undefined))


syntaxTests = TestList [TestLabel "ppTest1" ppTest1,
                        TestLabel "ppTest2" ppTest2,
                        TestLabel "ppTest3" ppTest3,
                        TestLabel "ppTest4" ppTest4,
                        TestLabel "ppTest5" ppTest5,
                        TestLabel "ppTest6" ppTest6,
                        TestLabel "ppTest7" ppTest7,
                        TestLabel "ppTest8" ppTest8,
                        TestLabel "ppTest9" ppTest9
                       ]


