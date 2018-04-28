
module Syntax where

import Utils

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad

import Debug.Trace

{-

  # Names

-}

data Name = PlaceHolder String
          | Priv String
          | FreshOut Int
          | FreshIn Int
          | Static String
          deriving (Eq, Ord)

ppName:: Name -> String
ppName (Static f) = f
ppName (PlaceHolder b) = "<" ++ b ++ ">"
ppName (Priv p) = "#" ++ p
ppName (FreshOut o) = "!" ++ (show o)
ppName (FreshIn i) = "?" ++ (show i)

instance Show Name where
  show = ppName

showNames:: [Name] -> String
showNames [] = ""
showNames [v] = (show v)
showNames (v:vs) = (show v) ++ ", " ++ (showNames vs)

namePublic:: Name -> Bool
namePublic (Priv _) = False
namePublic _ = True

namePrivate::Name -> Bool
namePrivate n = not (namePublic n)

nameFree::Name -> Bool
nameFree (PlaceHolder _) = False
nameFree _ = True

nameBound::Name -> Bool
nameBound n = not (nameFree n)

nameDynamic:: Name -> Bool
nameDynamic (PlaceHolder _) = False
nameDynamic (Priv _) = False
nameDynamic _ = True

clockOfName:: Name -> Int
clockOfName (FreshIn clk) = clk
clockOfName (FreshOut clk) = clk
clockOfName _ = 0   -- Note: 0 is a reserved clock value (= no clock)

{-

  # Process terms

-}

data Process a = Term a
               | Par (Process a) (Process a) a
               | Sum (Process a) (Process a) a
               | Step (Process a) a
               | Output Name Name (Process a) a
               | Input Name String (Process a) a
               | Restrict String (Process a) a
               | Match Name Name (Process a) a
               | Mismatch Name Name (Process a) a
               | Call String [Name] a

procInfo:: Process a -> a
procInfo (Term i) = i
procInfo (Par _ _ i) = i
procInfo (Sum _ _ i) = i
procInfo (Step _ i) = i
procInfo (Output _ _ _ i) = i
procInfo (Input _ _ _ i) = i
procInfo (Restrict _ _ i) = i
procInfo (Match _ _ _ i) = i
procInfo (Mismatch _ _ _ i) = i
procInfo (Call _ _ i) = i

ppProcess :: Process a -> String
ppProcess (Term  _) = "0"
ppProcess (Par p q _) = "(" ++ (ppProcess p) ++ " || " ++ (ppProcess q) ++ ")"
ppProcess (Sum p q _) = "(" ++ (ppProcess p) ++ " + " ++ (ppProcess q) ++ ")"
ppProcess (Step p _) = "tau." ++ (ppProcess p)
ppProcess (Output chan datum p _) =
  (ppName chan) ++ "!" ++ (ppName datum) ++ "." ++ (ppProcess p)
ppProcess (Input chan var p _) =
  (ppName chan) ++ "?(" ++ var ++ ")." ++ (ppProcess p)
ppProcess (Restrict r p _) = "new(" ++ r ++ "){" ++ (ppProcess p) ++ "}"
ppProcess (Match a b p _) = "[" ++ (ppName a) ++ "=" ++ (ppName b) ++ "] " ++ (ppProcess p)
ppProcess (Mismatch a b p _) = "[" ++ (ppName a) ++ "<>" ++ (ppName b) ++ "] " ++ (ppProcess p)
ppProcess (Call def args _) = def ++ "(" ++ (showNames args) ++ ")"

instance Show (Process a) where
  show = ppProcess

{-

 ## Process simplification

-}


simplifyProc1 :: Process a -> Process a
simplifyProc1 (Par (Term _) p _) = p
simplifyProc1 (Par p (Term _) _) = p
simplifyProc1 (Sum (Term _) p _) = p
simplifyProc1 (Sum p (Term _) _) = p
simplifyProc1 (Match a b p _) | a == b = p
simplifyProc1 (Mismatch a b p i) | a == b = (Term i)
simplifyProc1 p@(Restrict var q _)
  | (Set.member (PlaceHolder var) (freePlaceHolders q)) = p
  | otherwise = q
simplifyProc1 p = p

simplifyProc :: Process a -> Process a
simplifyProc (Par p q info) = simplifyProc1 (Par (simplifyProc p) (simplifyProc q) info)
simplifyProc (Sum p q info) = simplifyProc1 (Sum (simplifyProc p) (simplifyProc q) info)
simplifyProc (Step p info) = simplifyProc1 (Step (simplifyProc p) info)
simplifyProc (Output chan datum p info) = simplifyProc1 (Output chan datum (simplifyProc p) info)
simplifyProc (Input chan var p info) = simplifyProc1 (Input chan var (simplifyProc p) info)
simplifyProc (Match a b p info) = simplifyProc1 (Match a b (simplifyProc p) info)
simplifyProc (Mismatch a b p info) = simplifyProc1 (Mismatch a b (simplifyProc p) info)
simplifyProc (Restrict var p info) = simplifyProc1 (Restrict var (simplifyProc p) info)
simplifyProc p = simplifyProc1 p

compareProc:: Process a -> Process a -> Ordering
compareProc (Term _) (Term _) = EQ
compareProc (Term _) _ = LT
compareProc _ (Term _) = GT
compareProc (Call def args _) (Call def' args' _) =
  (compare def def') `neqOr` (compare args args')
compareProc (Call _ _ _) _ = LT
compareProc _ (Call _ _ _) = GT
compareProc (Step p _) (Step p' _) = compareProc p p'
compareProc (Step _ _) _ = LT
compareProc _ (Step _ _) = GT
compareProc (Output chan datum p _) (Output chan' datum' p' _) =
  (compare chan chan') `neqOr` (compare datum datum') `neqOr` (compareProc p p')
compareProc (Output _ _ _ _) _ = LT
compareProc  _ (Output _ _ _ _) = GT
compareProc (Input chan var p _) (Input chan' var' p' _) =
  (compare chan chan') `neqOr` (compareFreshProc var p var' p')
compareProc (Input _ _ _ _) _ = LT
compareProc _ (Input _ _ _ _) = GT
compareProc (Match a b p _) (Match a' b' p' _) =
  (compare a a') `neqOr` (compare b b') `neqOr` (compareProc p p')
compareProc (Match _ _ _ _) _ = LT
compareProc _ (Match _ _ _ _) = GT
compareProc (Mismatch a b p _) (Mismatch a' b' p' _) =
  (compare a a') `neqOr` (compare b b') `neqOr` (compareProc p p')
compareProc (Mismatch _ _ _ _) _ = LT
compareProc _ (Mismatch _ _ _ _) = GT
compareProc (Restrict res p _) (Restrict res' p' _) = compareFreshProc res p res' p'
compareProc (Restrict _ _ _) _ = LT
compareProc _ (Restrict _ _ _) = GT
compareProc (Par p q _) (Par p' q' _) =
  (compare p p') `neqOr` (compare q q')
compareProc (Par _ _ _) _ = LT
compareProc _ (Par _ _ _) = GT
compareProc (Sum p q _) (Sum p' q' _) =
  (compare p p') `neqOr` (compare q q')
--compareProc (Sum _ _ _) _ = LT
-- compareProc _ (Sum _ _ _) = GT
-- compareProc p q = error $ "Don't know how to compare " ++ (show p) ++ " and " ++ (show q)



compareFreshProc:: String -> Process a -> String -> Process a -> Ordering
compareFreshProc var p var' p' =
  let nvar = genFreshVar var (Par p p' (procInfo p))
  in compareProc (substProc p (Map.singleton (PlaceHolder var) (PlaceHolder nvar)))
                 (substProc p' (Map.singleton (PlaceHolder var') (PlaceHolder nvar)))

instance Eq (Process a) where
  (==) p q = (compareProc (simplifyProc p) (simplifyProc q)) == EQ 

instance Ord (Process a) where
  compare p q = compareProc (simplifyProc p) (simplifyProc q)

{-

  ## Well-formed processes

-}

wfName:: Name -> Set String -> Either String Bool
wfName n@(PlaceHolder v) vars
  | Set.member v vars = Right True
  | otherwise = Left $ "Placeholder '" ++ (show n) ++ "' not bound"
wfName _ _ = Right True

wfNames:: [Name] -> Set String -> Either String Bool
wfNames [] _ = Right True
wfNames (n:ns) vars = (wfName n vars) >> (wfNames ns vars)

wfProcess (Term _) _ = Right True
wfProcess (Par p q _) vars = (wfProcess p vars) >> (wfProcess q vars)
wfProcess (Sum p q _) vars = (wfProcess p vars) >> (wfProcess q vars)
wfProcess (Step p _) vars = wfProcess p vars
wfProcess (Output chan datum p _) vars  =
  (wfName chan vars) >> (wfName datum vars) >> (wfProcess p vars)
wfProcess (Input chan var p _) vars =
  (wfName chan vars) >> wfProcess p (Set.insert var vars)
wfProcess (Match a b p _) vars =
  (wfName a vars) >> (wfName b vars) >> (wfProcess p vars)
wfProcess (Mismatch a b p _) vars =
  (wfName a vars) >> (wfName b vars) >> (wfProcess p vars)
wfProcess (Restrict var p _) vars = wfProcess p (Set.insert var vars)
wfProcess (Call def args _) vars = wfNames args vars


{-

  # Process definitions

-}

data Def a = Def String [String] (Process a) a

ppParams :: [String] -> String
ppParams [] = ""
ppParams [p] = p
ppParams (p:ps) = p ++ ", " ++ (ppParams ps)

ppDef :: Def a -> String
ppDef (Def def params p _) = 
  def ++ "(" ++ (ppParams params) ++ ") = " ++ (show p)

instance Show (Def a) where
  show = ppDef

type DefEnv a = Map String (Def a)

{-

  # Free and bound names

-}

collectNames::Process a-> (Name -> Bool) -> Set Name
collectNames (Term _) _ = Set.empty
collectNames (Par p q _) pred = Set.union (collectNames p pred) (collectNames q pred)
collectNames (Sum p q _) pred = Set.union (collectNames p pred) (collectNames q pred)
collectNames (Step p _) pred = collectNames p pred
collectNames (Output chan datum p _) pred =
  let names = collectNames p pred in
  let names' = if pred chan then Set.insert chan names else names in
  let names'' = if pred datum then Set.insert datum names' else names' in
  names''
collectNames (Input chan _ p _) pred =
  let names = collectNames p pred in
  let names' = if pred chan then Set.insert chan names else names in
  names'
collectNames (Match a b p _) pred =
  let names = collectNames p pred in
  let names' = if pred a then Set.insert a names else names in
  let names'' = if pred b then Set.insert b names' else names' in
  names''
collectNames (Mismatch a b p _) pred =
  let names = collectNames p pred in
  let names' = if pred a then Set.insert a names else names in
  let names'' = if pred b then Set.insert b names' else names' in
  names''
collectNames (Restrict _ p _) pred = collectNames p pred
collectNames (Call _ args _) pred = foldl foldme Set.empty args 
  where foldme bs n = if pred n then Set.insert n bs else bs

freePlaceHoldersOfName:: Name -> Set Name
freePlaceHoldersOfName n@(PlaceHolder _) = Set.singleton n
freePlaceHoldersOfName _ = Set.empty

freePlaceHolders::Process a -> Set Name
freePlaceHolders (Term _) = Set.empty
freePlaceHolders (Par p q _) = Set.union (freePlaceHolders p) (freePlaceHolders q)
freePlaceHolders (Sum p q _) = Set.union (freePlaceHolders p) (freePlaceHolders q)
freePlaceHolders (Step p _) = freePlaceHolders p
freePlaceHolders (Output chan datum p _) =
  Set.union (freePlaceHoldersOfName chan) $
  Set.union (freePlaceHoldersOfName datum) $
  (freePlaceHolders p)
freePlaceHolders (Input chan var p _) =
  Set.union (freePlaceHoldersOfName chan) $
  Set.delete (PlaceHolder var) (freePlaceHolders p) 
freePlaceHolders (Match a b p _) =
  Set.union (freePlaceHoldersOfName a) $
  Set.union (freePlaceHoldersOfName b) $
  (freePlaceHolders p)
freePlaceHolders (Mismatch a b p _) =
  Set.union (freePlaceHoldersOfName a) $
  Set.union (freePlaceHoldersOfName b) $
  (freePlaceHolders p)
freePlaceHolders (Restrict var p _) = Set.delete (PlaceHolder var) (freePlaceHolders p)
freePlaceHolders (Call _ args _) = foldl foldme Set.empty args 
  where foldme bs n@(PlaceHolder _) = Set.insert n bs
        foldme bs _ = bs

freeNames:: Process a -> Set Name
freeNames p = collectNames p nameFree

boundNames:: Process a -> Set Name
boundNames p = collectNames p nameBound

allNames:: Process a -> Set Name
allNames p = collectNames p (\_ -> True)

privateNames:: Process a -> Set Name
privateNames p = collectNames p namePrivate

publicNames:: Process a -> Set Name
publicNames p = collectNames p namePublic

dynamicNames:: Process a -> Set Name
dynamicNames p = collectNames p nameDynamic

{-

 # Process substitution

-}

substName:: Name -> Map Name Name -> Name
substName n env =
  case Map.lookup n env of
    Just m -> m
    Nothing -> n

{- -- doesn't work for private names
substName n@(PlaceHolder _) env =
  case Map.lookup n env of
    Just m -> m
    Nothing -> n
substName n _ = n
-}

substProc:: Process a -> Map Name Name -> Process a
substProc (Term info) _ = Term info
substProc (Par p q info) env = Par (substProc p env) (substProc q env) info
substProc (Sum p q info) env = Sum (substProc p env) (substProc q env) info
substProc (Step p info) env = Step (substProc p env) info
substProc (Output chan datum p info) env =
  Output (substName chan env) (substName datum env) (substProc p env) info

substProc (Input chan var p info) env =
  Input (substName chan env) var (substProc p (Map.delete (PlaceHolder var) env)) info

substProc (Match a b p info) env =
  Match (substName a env) (substName b env) (substProc p env) info

substProc (Mismatch a b p info) env =
  Mismatch (substName a env) (substName b env) (substProc p env) info

substProc (Restrict res p info) env =
  Restrict res (substProc p (Map.delete (PlaceHolder res) env)) info

substProc (Call def args info) env =
  Call def (map (\arg -> substName arg env) args) info


{-

 # Process enclosing

-}

encloseProc:: Set String -> Process a -> Process a
encloseProc _ (Term info) = Term info
encloseProc bound (Par p q info) = Par (encloseProc bound p) (encloseProc bound q) info
encloseProc bound (Sum p q info) = Sum (encloseProc bound p) (encloseProc bound q) info
encloseProc bound (Step p info) = Step (encloseProc bound p) info
encloseProc bound (Output chan datum p info) =
  Output (encloseName bound chan) (encloseName bound datum) (encloseProc bound p) info
encloseProc bound (Input chan var p info) =
  Input (encloseName bound chan) var (encloseProc (Set.insert var bound) p) info
encloseProc bound (Match a b p info) =
  Match (encloseName bound a) (encloseName bound b) (encloseProc bound p) info
encloseProc bound (Mismatch a b p info) =
  Mismatch (encloseName bound a) (encloseName bound b) (encloseProc bound p) info
encloseProc bound (Restrict res p info) =
  Restrict res (encloseProc (Set.insert res bound) p) info
encloseProc bound (Call def args info) =
  Call def (map (\arg -> encloseName bound arg) args) info

encloseName:: Set String -> Name -> Name
encloseName bound (Static v)
  | Set.member v bound = PlaceHolder v
  | otherwise = Static v
encloseName _ n = n

{-

 # Misc. syntax utilities

-}

genPriv:: String -> Process a -> Name
genPriv prefix p =
  let bs = privateNames p in
  if Set.member (Priv prefix) bs
  then genPriv (prefix ++ "'") p
  else (Priv prefix)

genFreshVar:: String -> Process a -> String
genFreshVar prefix p =
  let bs = boundNames p
  in if Set.member (PlaceHolder prefix) bs
     then genFreshVar (prefix ++ "'") p
     else prefix


isCallProcess:: Process a -> Bool
isCallProcess (Par p q _) = (isCallProcess p) || (isCallProcess q)
isCallProcess (Sum p q _) = (isCallProcess p) || (isCallProcess q)
isCallProcess (Call _ _ _) = True
isCallProcess _ = False

