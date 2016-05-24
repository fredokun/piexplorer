
module Symbolic where

import Utils

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Syntax

import Debug.Trace

{-

  # Symbolic transitions

-}

{-

  ## Labels

-}

data Label = Tau
           | Out Name Name
           | BoundOut Name Name
           | In Name String
           deriving (Eq, Ord)

showLabel:: Label -> String
showLabel Tau = "tau"
showLabel (Out chan datum) = (show chan) ++ "!" ++ (show datum)
showLabel (In chan var) = (show chan) ++ "?(" ++ var ++ ")"
showLabel (BoundOut chan datum) = (show chan) ++ "!(" ++ (show datum) ++ ")"

instance Show Label where
  show = showLabel

{-

  ## Guards

-}

data Guard = GTrue
           | GEq Name Name
           | GIneq Name Name
           | GConj Guard Guard

showGuard:: Guard -> String
showGuard g = showAux g True
  where showAux GTrue True = "true"
        showAux GTrue _ = ""
        showAux (GEq a b) _ = (show a) ++ "=" ++ (show b)
        showAux (GIneq a b) _ = (show a) ++ "<>" ++ (show b)
        showAux (GConj g h) _ = (showAux g True) ++ "^" ++ (showAux h False)

instance Show Guard where
  show = showGuard


simplifyGuard1:: Guard -> Guard
simplifyGuard1 g@(GEq a b)
  | a == b = GTrue
  | otherwise = g
simplifyGuard1 (GConj GTrue g) = g
simplifyGuard1 (GConj g GTrue) = g
simplifyGuard1 g = g

simplifyGuard:: Guard -> Guard
simplifyGuard (GConj g1 g2) = simplifyGuard1 (GConj (simplifyGuard g1) (simplifyGuard g2))
simplifyGuard g = simplifyGuard1 g

compareGuard:: Guard -> Guard -> Ordering
compareGuard g h = compareGuard' (simplifyGuard g) (simplifyGuard h)
  where
    compareGuard' GTrue GTrue = EQ
    compareGuard' GTrue _ = LT
    compareGuard' _ GTrue = GT
    compareGuard' (GEq a b) (GEq a' b') = (compare a a') `neqOr` (compare b b')
    compareGuard' (GEq _ _) _ = LT
    compareGuard' _ (GEq _ _) = GT
    compareGuard' (GIneq a b) (GIneq a' b') = (compare a a') `neqOr` (compare b b')
    compareGuard' (GIneq _ _) _ = LT
    compareGuard' _ (GIneq _ _) = GT
    compareGuard' (GConj g h) (GConj g' h') = (compare g g') `neqOr` (compare h h') 

instance Eq Guard where
  (==) g h = (compareGuard g h) == EQ 

instance Ord Guard where
  compare = compareGuard


data Kind = Internal | External
          deriving (Eq, Ord)

instance Show Kind where
  show Internal = "<intern>"
  show External = ""

data SymTrans a = SymTrans Kind Guard Label (Process a)
              deriving (Eq, Ord)

showSymTrans:: SymTrans a -> String
showSymTrans (SymTrans k g l p)
  = (show k) ++ "--[" ++ (gstr g) ++ (show l) ++ "]--> " ++ (show p)
  where gstr GTrue = ""
        gstr g = (show g) ++ ","

instance Show (SymTrans a) where
  show = showSymTrans

{-

  # Semantic rules

-}

newtype SymbolicError = SymbolicError String

symStep:: Process a -> (DefEnv a) -> Either SymbolicError (Set (SymTrans a))
symStep (Term _) _ = Right $ Set.empty

symStep (Step p _) _ = Right $ Set.singleton  (SymTrans External GTrue Tau (simplifyProc p))

symStep (Output chan datum p _) _
  | namePublic datum =
      let kind = if namePublic chan then External else Internal
      in
        Right $ Set.singleton (SymTrans kind GTrue (Out chan datum) (simplifyProc p))

  | namePublic chan = -- remark: datum is private
      Right $ Set.insert (SymTrans Internal GTrue (Out chan datum)  (simplifyProc p)) $
      Set.singleton (SymTrans External GTrue (BoundOut chan datum) (simplifyProc p))

  | otherwise = -- remark: both chan and datum are private
      Right $ Set.singleton (SymTrans Internal GTrue (Out chan datum)  (simplifyProc p))

symStep (Input chan var p _) _
  | namePublic chan = Right $ Set.singleton (SymTrans External GTrue (In chan var) (simplifyProc p))
  | otherwise = Right $ Set.singleton (SymTrans Internal GTrue (In chan var) (simplifyProc p))

symStep (Match a b p _) defEnv =
  do nexts <- symStep p defEnv
     Right (Set.map doMatch nexts)
     where doMatch (SymTrans k g l p) = SymTrans k (GConj (GEq a b) g) l p

symStep (Mismatch a b p _) defEnv =
  do nexts <- symStep p defEnv
     Right (Set.map doMatch nexts)
     where doMatch (SymTrans k g l p) = SymTrans k (GConj (GIneq a b) g) l p

symStep (Restrict var p _) defEnv =
  let npriv = genPriv var p in
  let substp = substProc p (Map.singleton (PlaceHolder var) npriv)
  in -- trace ("subst proc = " ++ (show substp))
     symStep (simplifyProc substp) defEnv

symStep (Par p q info) defEnv =
  do lefts <- symStep p defEnv
     rights <- symStep q defEnv
     let syncs = syncStep lefts rights
     Right (Set.union (Set.map leftPar lefts) (Set.union (Set.map rightPar rights) syncs))
       where leftPar (SymTrans k g l p) = SymTrans k g l (simplifyProc (Par p q info))
             rightPar (SymTrans k g l q) = SymTrans k g l (simplifyProc (Par p q info))

symStep (Sum p q _) defEnv =
  do lefts <- symStep p defEnv
     rights <- symStep q defEnv
     Right (Set.union lefts rights)

symStep (Call defName args _) defEnv =
  case Map.lookup defName defEnv of
    Just (Def _ params p _) ->
      case makeCall params args p of
      Left err -> Left err
      Right p' -> symStep p' defEnv
    Nothing -> Left $ SymbolicError ("No such definition: " ++ defName)

makeCall:: [String] -> [Name] -> Process a -> Either SymbolicError (Process a)
makeCall params args p =
  case makeArgs params args Map.empty of
  Left err -> Left err
  Right env -> Right $ substProc p env
  where makeArgs:: [String] -> [Name] -> Map Name Name -> Either SymbolicError (Map Name Name)
        makeArgs [] [] env = Right env
        makeArgs [] _ _ = Left $ SymbolicError "Too many arguments in call"
        makeArgs _ [] _ = Left $ SymbolicError "Not enough arguments in call"
        makeArgs (p:ps) (a:as) env = makeArgs ps as (Map.insert (PlaceHolder p) a env) 

-- Note: too bad we cannot have a Data.Set monad ...
syncStep:: Set (SymTrans a) -> Set (SymTrans a) -> Set (SymTrans a)
syncStep lefts rights  = 
  Set.fold syncRight Set.empty lefts
  where syncRight left syncs = 
          Set.fold (\right rsyncs -> 
                      case (doSync left right) of
                      Just t -> Set.insert t rsyncs
                      Nothing -> rsyncs) syncs rights

doSync:: (SymTrans a) -> (SymTrans a) -> Maybe (SymTrans a)
doSync (SymTrans _ gout (Out outChan datum) pout) (SymTrans _ gin (In inChan var) pin)
  = Just (SymTrans External (simplifyGuard (GConj (GEq outChan inChan) (GConj gout gin))) Tau
          (simplifyProc (Par pout (substProc pin (Map.singleton (PlaceHolder var) datum)) (procInfo pout))))
doSync pin@(SymTrans _ _ (In _ _) _) pout@(SymTrans _ _ (Out _ _) _)
  = doSync pout pin
doSync _ _ = Nothing

