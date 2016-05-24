
module Dist where

import Data.List

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Utils
import Rel

import Syntax

data Dist = Dist (Rel Name Name) deriving (Eq, Ord)

showDist:: Dist -> String
showDist (Dist d) = concats $ intersperse "," (foldValRel foldShow [] d)
  where -- foldShow:: Name -> Name -> [String] -> [String]
        foldShow n m strs = [(show n) ++ "<>" ++ (show m)] ++ strs

instance Show Dist where
  show d = "Dist{" ++ (showDist d) ++ "}"

emptyDist :: Dist
emptyDist = Dist emptyRel

filterInvDist:: Dist -> Dist
filterInvDist (Dist d) = Dist $ filterRel (\n m -> n < m) d

-- foldRel:: (Name -> Set Name -> Dist -> Dist) -> init:Dist -> Dist -> Dist
collectInvDist:: Dist -> Dist
collectInvDist (Dist d) =
  Dist (foldValRel (\n m d -> if m < n then (insertRel m n d) else d) emptyRel d)

mergeDist:: Dist -> Dist -> Dist
mergeDist (Dist d1) (Dist d2) = Dist (mergeRel d1 d2)

normalizeDist :: Dist -> Dist
normalizeDist (Dist d) = mergeDist (filterInvDist (Dist d)) (collectInvDist (Dist d))

distinct:: Name -> Name -> Dist -> Bool
distinct n m (Dist d)
  | n == m = False
  | n < m = inCodomain n m d
  | otherwise = inCodomain m n d

distinguish:: Name -> Name -> Dist -> Dist
distinguish n m (Dist d)
  | n == m = error $ "distinguish: Cannot distinguish identical names: " ++ (show n)
  | n < m = Dist $ insertRel n m d
  | otherwise = Dist $ insertRel m n d

removeName:: Name -> Dist -> Dist
removeName n (Dist d) = Dist $ removeValue n (removeArgument n d)

replaceName:: Name -> Name -> Dist -> Dist
replaceName n m (Dist d)
  | n == m = Dist d
  | inDomain m d = error $ "replaceName: Cannot replace '" ++ (show n) ++ "' with '" ++ (show m) ++ "' (already in domain)"
  | otherwise = normalizeDist $ Dist (substCodomain n m (substDomain n m d))


cleanupDist:: Dist -> Set Name -> Dist
cleanupDist (Dist d) ns = Dist $ Map.foldrWithKey cleanup Map.empty d
  where cleanup:: Name -> Set Name -> Rel Name Name -> Rel Name Name
        cleanup key vals res
          | not (Set.member key ns) = res
          | otherwise = let vals' = Set.intersection vals ns
                        in if Set.null vals' then res
                           else Map.insert key vals' res


namesInDist:: Dist -> Set Name
namesInDist (Dist dist) = foldRel (\key vals res -> Set.insert key (Set.union vals res)) Set.empty dist

allDistinct:: Name -> Dist -> Dist
allDistinct newName (Dist dist) =
  let (dist', dist'') = foldRel allDist (emptyRel, emptyRel) dist
  in Dist $ mergeRel dist' dist''
  where allDist:: Name -> Set Name -> (Rel Name Name, Rel Name Name) -> (Rel Name Name, Rel Name Name)
        allDist n ns (update_d, new_d)
          | n == newName = error $ "allDistinct: Cannot distinguish \"newName\" '" ++ (show newName) ++ "': already present in distinctions."
          | n < newName = (insertVals n (Set.insert newName ns) update_d, new_d)
          | otherwise = (update_d, insertRel newName n new_d)

