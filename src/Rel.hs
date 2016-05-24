
module Rel where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

type Rel k a = Map k (Set a)

emptyRel:: Rel k a
emptyRel = Map.empty

insertRel:: Ord k => Ord a => k -> a -> Rel k a -> Rel k a
insertRel k v m = Map.alter alterf k m
  where alterf Nothing = Just $ Set.singleton v
        alterf (Just vs) = Just (Set.insert v vs)

mergeRel:: Ord k => Ord a => Rel k a -> Rel k a -> Rel k a
mergeRel d1 d2 = Map.unionWith Set.union d1 d2

foldRel:: (Ord k, Ord a) => (k -> Set a -> b -> b) -> b -> Rel k a -> b
foldRel f unit r = Map.foldrWithKey f unit r

foldValRel:: (Ord k, Ord a) => (k -> a -> b -> b) -> b -> Rel k a -> b
foldValRel f unit r = foldRel foldKey unit r
  where --foldKey:: k -> Set a -> b -> b
        foldKey key vs res = Set.foldr foldVal res vs
          where --foldVal:: a -> b -> b
                foldVal v res = f key v res

filterRel:: (Ord k, Ord a) => (k -> a -> Bool) -> Rel k a -> Rel k a
filterRel pred r = Map.foldrWithKey filtKey emptyRel r
  where --filtKey:: k -> Set a -> Rel k a -> Rel k a
        filtKey key vs res =
          let fset = Set.filter (\w -> pred key w) vs
          in if Set.null fset then res
             else Map.insert key fset res

inDomain:: Ord k => k -> Rel k a -> Bool
inDomain = Map.member

inCodomain:: (Ord k, Ord a) => k -> a -> Rel k a -> Bool
inCodomain key val r =
  case Map.lookup key r of
    Nothing -> False
    Just vals -> Set.member val vals
    
removeArgument:: Ord k => k -> Rel k a -> Rel k a
removeArgument = Map.delete

removeValue:: (Ord k, Ord a) => a -> Rel k a -> Rel k a
removeValue val rel = Map.foldrWithKey fremove emptyRel rel
  where -- fremove:: k -> a -> Rel k a -> Rel k a
        fremove key vals rel = Map.insert key (Set.delete val vals) rel

        
findVal:: Ord k => k -> Rel k a -> Maybe (Set a)
findVal = Map.lookup

insertVals:: (Ord k, Ord a) => k -> Set a -> Rel k a -> Rel k a
insertVals = Map.insert 

substDomain:: (Ord k, Ord a) => k -> k -> Rel k a -> Rel k a
substDomain keyOld keyNew rel =
  case findVal keyOld rel of
    Nothing -> rel
    Just vals -> insertVals keyNew vals (removeArgument keyOld rel)

substCodomain:: (Ord k, Ord a) => a -> a -> Rel k a -> Rel k a
substCodomain valOld valNew rel = foldRel fsubst emptyRel rel
  where fsubst key vals res =
          if Set.member valOld vals
          then insertVals key (Set.insert valNew (Set.delete valOld vals)) res
          else insertVals key vals res
