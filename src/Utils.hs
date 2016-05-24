
module Utils where

import Data.Set (Set)
import qualified Data.Set as Set

neqOr:: Ordering -> Ordering -> Ordering
neqOr EQ ord = ord
neqOr ord _ = ord

startsWith:: Eq a => [a] -> [a] -> Bool
_ `startsWith` [] = True
[] `startsWith` _ = False
(l:ls) `startsWith` (r:rs) =
  (l == r) && (ls `startsWith` rs)


concats:: [[a]] -> [a]
concats [] = []
concats (l:ls) = l ++ (concats ls)

