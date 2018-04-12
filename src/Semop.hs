
module Semop where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Syntax
import Symbolic

import Env

import Debug.Trace

data State a = State Env (Process a) deriving (Eq, Ord)


showState:: State a -> String
showState (State env p) = (showEnv env) ++ " |- " ++ (ppProcess p)

instance Show (State a) where
  show = showState

data Action = TauAct
            | OutAct Name Name
            | InAct Name Name
            | EscapeAct Name Name
            deriving (Eq, Ord)

showAction:: Action -> String
showAction TauAct = "tau"
showAction (OutAct chan datum) = (show chan) ++ "<" ++ (show datum) ++ ">"
showAction (InAct chan input) = (show chan) ++ "(" ++ (show input) ++ ")"
showAction (EscapeAct chan name) = (show chan) ++ "<#" ++ (show name) ++ ">"

data Transition a = Transition Action (State a) deriving (Eq, Ord)

showTrans:: Transition a -> String
showTrans (Transition act st) = "--[" ++ (showAction act) ++ "]--> " ++ (showState st)

instance Show (Transition a) where
  show t = "Trans{" ++ (showTrans t) ++ "}"

initState:: Process a -> State a
initState p = State (initEnv (allNames p)) p

refineEnv:: Env -> Guard -> Maybe Env
refineEnv env GTrue = Just env
refineEnv env (GEq n m) = if matchable n m env
                          then equalizeNames n m env
                          else Nothing
refineEnv env (GIneq n m) = if mismatchable n m env
                            then distinguishNames n m env
                            else Nothing
refineEnv env (GConj g1 g2) = do env' <- refineEnv env g1
                                 refineEnv env' g2 

liftTrans:: State a -> SymTrans a -> Maybe (Transition a)
liftTrans (State env p) (SymTrans External g act q) =
  case refineEnv env g of
  Nothing -> Nothing
  Just env' -> Just $ doAct env' act q

    where doAct:: Env -> Label -> Process a -> Transition a
          doAct env Tau q = 
            Transition TauAct (State (clean env q) q)

          doAct env (Out chan datum) q = 
            Transition (OutAct
                        (envRealName chan env)
                        (envRealName datum env))
            (State (clean env q) q)

          doAct env (In chan var) q = 
            let (env', freshIn) = -- trace "Generate fresh input" $
                  genFreshInput env
            in let q' = -- trace (" ==> updated env' = " ++ (show env')) $
                     substProc q (Map.singleton (PlaceHolder var) freshIn)
                   env'' = -- trace (" ==> updated env'' = " ++ (show (clean env' q'))) $
                     clean env' q'
               in
                 (Transition
                  (InAct (envRealName chan env) freshIn)
                  (State env'' q'))

          doAct env (BoundOut chan priv) q = 
            let (env', freshOut) = -- trace "Generate fresh output" $
                  genFreshOutput env
            in
              let q' = -- trace (" ==> updated env' = " ++ (show env')) $
                    substProc q (Map.singleton priv freshOut)
                  env'' = -- trace (" ==> updated env'' = " ++ (show (clean env' q'))) $
                    clean env' q'
               in
                 -- trace (" ==> next proc = " ++ (show q'))
                 (Transition
                  (EscapeAct (envRealName chan env) freshOut)
                  (State env'' q'))

  where clean env p = cleanupEnv env (dynamicNames p)

liftTrans (State _ _) (SymTrans Internal _ _ _) = Nothing

semTrans:: State a -> Set (SymTrans a) -> Set (Transition a)
semTrans s steps =
  Set.foldr semLift Set.empty steps
  where -- semLift:: SymTrans a -> Set (Transition a) -> Set (Transition a)
        semLift step trans =
          case liftTrans s step of
          Nothing -> trans
          Just t -> Set.insert t trans
