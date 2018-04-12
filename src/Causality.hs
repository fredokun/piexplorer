
module Causality where

import Data.List

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Utils
import Syntax

data CausalOrder = CausalOrder (Map Name (Set Name)) deriving (Eq, Ord)

showCausal :: CausalOrder -> String
showCausal (CausalOrder cs) = concats $ intersperse "," (Map.foldrWithKey foldShow [] cs)
  where foldShow:: Name -> (Set Name) -> [String] -> [String]
        foldShow o is strs = [(show o) ++ "<" ++ (showAll is)] ++ strs

        showAll:: (Set Name) -> String
        showAll is = "{" ++ (concats $ intersperse "," (map show (Set.toList is))) ++ "}"

instance Show CausalOrder where
  show c = "CausalOrder{" ++ (showCausal c) ++ "}"

emptyCausal :: CausalOrder
emptyCausal = CausalOrder Map.empty

causalFreshOut :: CausalOrder -> Name -> CausalOrder
causalFreshOut (CausalOrder cs) o@(FreshOut _) = CausalOrder $ Map.insert o Set.empty cs
causalFreshOut _ _ = error "causalFreshOut only works for fresh outputs"

causalFreshIn :: CausalOrder -> Name -> CausalOrder
causalFreshIn (CausalOrder cs) i@(FreshIn _) = CausalOrder $ Map.foldrWithKey (foldIn i) Map.empty cs
  where foldIn i o is cs = Map.insert o (Set.insert i is) cs
causalFreshIn _ _ = error "causalFreshIn only works for fresh inputs"  

causeOf :: CausalOrder -> Name -> Name -> Bool
causeOf (CausalOrder cs) o@(FreshOut _) i@(FreshIn _) =
  case Map.lookup o cs of
    Just is -> Set.member i is
    Nothing -> error $ "No such fresh output name: " ++ (show o)
causeOf _ _ _ = error "o `causeOf` i only works if o is fresh output and i a fresh input"

causalCollect :: CausalOrder -> Name -> CausalOrder
causalCollect (CausalOrder cs) o@(FreshOut _) =
  let _ = cs ! o
  in CausalOrder $ Map.delete o cs
causalCollect (CausalOrder cs) i@(FreshIn _) =
  CausalOrder $ Map.foldrWithKey foldCollect Map.empty cs
  where foldCollect :: Name -> (Set Name) -> (Map Name (Set Name)) -> (Map Name (Set Name))
        foldCollect o is cs = Map.insert o (Set.delete i is) cs
causalCollect cs _ = cs
        
cleanupCausal :: CausalOrder -> Set Name -> CausalOrder
cleanupCausal causal names = Set.fold  (\n causal -> causalCollect causal n) causal names

causalReplaceName :: Name -> Name -> CausalOrder -> CausalOrder
causalReplaceName i1@(FreshIn _) i2@(FreshIn _) (CausalOrder cs) =
  CausalOrder $ Map.foldrWithKey foldReplace Map.empty cs
  where foldReplace o is cs  = if Set.member i1 is
                               then Map.insert o (Set.insert i2 (Set.delete i1 is)) cs
                               else Map.insert o is cs
causalReplaceName i@(FreshIn _) o@(FreshOut _) (CausalOrder cs) =
  CausalOrder $ Map.foldrWithKey foldRemove Map.empty cs
  where foldRemove o is cs = Map.insert o (Set.delete i is) cs
causalReplaceName _ _ causal = causal
