
import Test.HUnit


import qualified SyntaxTests
import qualified DistTests
import qualified SemopTests


main =
  do runTestTT SyntaxTests.syntaxTests
     runTestTT DistTests.distTests
     runTestTT SemopTests.semopTests

