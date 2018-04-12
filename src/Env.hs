
module Env where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

import Syntax
import Utils

import Equiv
import Dist
import Causality

import Debug.Trace

data Env = Env Equiv Dist CausalOrder deriving (Eq, Ord)

showEnv:: Env -> String
showEnv (Env eq dist causal) =
  concats $ intersperse "," [(showEquiv eq), (showDist dist), (showCausal causal)]
  
instance Show Env where
  show e = "Env{" ++ (showEnv e) ++ "}"

initEnv:: Set Name -> Env
initEnv ns = Env (initEquiv ns) emptyDist emptyCausal

namesInEnv :: Env -> Set Name
namesInEnv (Env eq _ _) = namesInEquiv eq   -- Note: distinctions are about the names in equivalence

matchable :: Name -> Name -> Env -> Bool
matchable a b env@(Env eq dist _) =
  let a' = realName a eq
      b' = realName b eq
  in
    not (distinct a' b' dist) && (a' == b' || matchCases a' b' env || matchCases b' a' env)
  where matchCases (Static _) (Static _) env = True
        matchCases (Static _) (FreshIn _) env = True
        matchCases (FreshIn _) (FreshIn _) env = True
        matchCases i@(FreshIn _) o@(FreshOut _) (Env _ _ causal) = causeOf causal o i
        matchCases _ _ _ = False

mismatchable :: Name -> Name -> Env -> Bool
mismatchable a b env@(Env eq _ _) = not $ equalNames a b eq

envRealName:: Name -> Env -> Name
envRealName n (Env eq _ _) = realName n eq

equalizeNames:: Name -> Name -> Env -> Maybe Env
equalizeNames (Priv n) (Priv m) env
  | n == m = Just env -- equal private names (OK)
  | otherwise = Nothing -- distinct private names (KO)
equalizeNames (Priv _) _ _ = Nothing -- left only is private (KO)
equalizeNames _ (Priv _) _ = Nothing -- right only is private (KO)
equalizeNames (Static _) (FreshOut _) _ = Nothing -- cannot equalize static/fresh out (KO)
equalizeNames (FreshOut _) (Static _) _ = Nothing -- symmetric case (KO)
equalizeNames n m env@(Env eq dist causal) =
  equalizer (realName n eq) (realName m eq)
  where equalizer real_n real_m
          | real_n == real_m = Just env --already equal
          | distinct real_n real_m dist = Nothing -- cannot equalize distinct names
          | real_n < real_m =
              Just $ Env (equalize real_n real_m eq) (replaceName real_m real_n dist) (causalReplaceName real_m real_n causal)
           | otherwise = Just $ Env (equalize real_m real_n eq) (replaceName real_n real_m dist) (causalReplaceName real_n real_m causal)


distinguishNames:: Name -> Name -> Env -> Maybe Env
distinguishNames (Priv n) (Priv m) env
  | n == m = Nothing -- equal private names (KO)
  | otherwise = Just env -- distinct private names (OK)
distinguishNames (Priv _) _ env = Just env -- left only is private (OK)
distinguishNames _ (Priv _) env = Just env -- right only is private (OK)
-- general case
distinguishNames n m env@(Env eq dist causal) =
  distinguisher (realName n eq) (realName m eq)
  where distinguisher real_n real_m
          | real_n == real_m = Nothing -- Cannot distinguish equal names
          | otherwise = Just $ Env eq (distinguish real_n real_m dist) causal

cleanupEnv:: Env -> Set Name -> Env
cleanupEnv env@(Env eq dist causal) names =
  let env' = Env (cleanupEquiv eq names) (cleanupDist dist names) (cleanupCausal causal names)
  in
    (trace ("cleanupEnv env="  ++ (show env) ++ " names=" ++ (show names)))
    (trace ("  ==> env'=" ++ (show env')))
    env'


computeClockValue:: Env -> Int
computeClockValue env = searchClock 1 (Set.toList (Set.map clockOfName (namesInEnv env)))
  where searchClock:: Int -> [Int] -> Int
        searchClock n [] = n
        searchClock n (m:ms)
          | m == 0 = searchClock n ms
          | n == m = searchClock (n + 1) ms
          | otherwise = n


genFreshInput:: Env -> (Env, Name)
genFreshInput env@(Env eq dist causal) =
  let freshIn = FreshIn (computeClockValue env)
  in
    ( Env (newName freshIn eq) dist (causalFreshIn causal freshIn), freshIn)

genFreshOutput:: Env -> (Env, Name)
genFreshOutput env@(Env eq dist causal) =
  let freshOut = FreshOut (computeClockValue env)
  in
    ( (Env (newName freshOut eq) (allDistinct freshOut dist) (causalFreshOut causal freshOut)), freshOut )



