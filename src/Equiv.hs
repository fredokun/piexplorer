
module Equiv where

import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Utils

import Syntax

import Debug.Trace

data Equiv = Equiv (Map Name Name) deriving (Eq, Ord)

showEquiv:: Equiv -> String
showEquiv (Equiv eq) = concats $ intersperse "," (Map.foldrWithKey foldShow [] eq)
  where foldShow:: Name -> Name -> [String] -> [String]
        foldShow n m strs = [(show n) ++ "=" ++ (show m)] ++ strs

instance Show Equiv where
  show eq = "Equiv{" ++ (showEquiv eq) ++ "}"

emptyEquiv:: Equiv
emptyEquiv = Equiv Map.empty

newName:: Name -> Equiv -> Equiv
newName n (Equiv eq) = Equiv $ Map.insert n n eq

initEquiv:: Set Name -> Equiv
initEquiv ns = Set.fold finit emptyEquiv ns
  where finit:: Name -> Equiv -> Equiv
        finit n@(Static _) res = newName n res
        finit _ res = res

realName:: Name -> Equiv -> Name
realName n@(Priv _) _ = n
realName n (Equiv eq) =
  -- trace ("realName: " ++ (show n) ++ " in " ++ (show eq) ++ " ?") $
  case Map.lookup n eq of
  Nothing -> error $ "realName: Name '" ++ (show n) ++ "' unknown to the environment."
  Just m | m == n -> n
         | otherwise -> realName m (Equiv eq)

equalNames:: Name -> Name -> Equiv -> Bool
equalNames n m eq = (realName n eq) == (realName m eq)

equalize:: Name -> Name -> Equiv -> Equiv
equalize n m e@(Equiv eq) =
  let real_n = realName n e
      real_m = realName m e
  in
    if real_n == real_m
    then error $ "Names '" ++ (show n) ++ "' and '" ++ (show m) ++ "' already equalized"
    else (if real_n < real_m
          then Equiv $ Map.insert real_n real_m eq
          else Equiv $ Map.insert real_m real_n eq)

cleanupEquiv:: Equiv -> Set Name -> Equiv
cleanupEquiv (Equiv eq) ns =
  Equiv $ Map.foldrWithKey cleanup Map.empty eq
  where cleanup:: Name -> Name -> Map Name Name -> Map Name Name
        cleanup key val res =
          case (Set.member key ns, Set.member val ns) of
          (True, True) -> Map.insert key val res
          (True, False) -> Map.insert key key res
          (False, True) -> Map.insert val val res
          (False, False) -> res


namesInEquiv:: Equiv -> Set Name
namesInEquiv (Equiv eq) = Map.foldrWithKey (\key val res -> Set.insert val (Set.insert key res)) Set.empty eq


