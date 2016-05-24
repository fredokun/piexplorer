
module GenGraph where

import System.IO

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Syntax
import Symbolic
import Semop

import Debug.Trace

type FixedStates a = Map (Semop.State a) String

data GenState a = GenState { fixedStates:: FixedStates a
                           , nullState:: Maybe (State a)
                           , nbStates:: Int
                           , nbTrans:: Int }

genGraph:: Show a => Process a -> DefEnv a -> String -> Handle -> IO ()
genGraph p defEnv src hOut =
  do genHeader src hOut
     let gen = GenState { fixedStates = Map.empty
                          , nullState = Nothing
                          , nbStates = 0
                          , nbTrans = 0 }
     let init = initState p
     query <- genState init defEnv gen hOut
     gen' <- case query of
       Left _ -> error "genGraph: unexpected 'Left' (please report)"
       Right (stateName, gen') -> 
         genStateTrans stateName init defEnv gen' hOut
     genFooter gen' hOut

genHeader:: String -> Handle -> IO ()
genHeader src hOut = do hPutStrLn hOut "/* Graph generated by *pisym* tool */"
                        hPutStrLn hOut ("// @src(\"" ++ src ++ "\")")
                        hPutStrLn hOut "digraph {"

genFooter:: GenState a -> Handle -> IO ()
genFooter gen hOut = do hPutStrLn hOut "} // end of digraph"
                        hPutStrLn hOut ""
                        hPutStrLn hOut $ "// Number of states = " ++ (show (nbStates gen))
                        hPutStrLn hOut $ "// Number of transitions = " ++ (show (nbTrans gen))
                        hPutStrLn hOut ""

                        putStrLn $ "LTS generated with " ++ (show (nbStates gen)) ++ " states and " ++ (show (nbTrans gen)) ++ " transitions."
                        putStrLn $ "(stored " ++ (show (Map.size (fixedStates gen))) ++ " states for fixed-point detection)"


checkSaveState:: Process a -> Bool
--checkSaveState _ = True
checkSaveState = isCallProcess


fetchState:: State a -> GenState a -> Either String (String, GenState a)
fetchState st@(State _ (Term _)) gen =
  case nullState gen of
  Nothing -> Right  ("T", gen { nullState = Just st, nbStates = (nbStates gen) + 1 })
  Just _ -> Left "T"

fetchState st@(Semop.State _ p) gen =
  -- trace ("[fetchState]: state = " ++ (show st)) $
  case Map.lookup st (fixedStates gen) of
  Just stateName -> --trace ("[fetchState]: state found") $
    Left stateName
  Nothing -> --trace ("[fetchState]: state not found  (isCallProcess ? " ++ (show (isCallProcess p)) ++ ")") $
             let stateName = "S" ++ (show (nbStates gen))
             in if checkSaveState p
                then --trace ("[fetchState]: record state (call process)") $
                  Right (stateName, gen { fixedStates = (Map.insert st stateName (fixedStates gen)),
                                             nbStates = (nbStates gen) + 1 }) -- Note: only record call states
                else Right (stateName, gen { nbStates = (nbStates gen) + 1 })

genState:: Show a => State a -> DefEnv a -> GenState a -> Handle -> IO (Either String (String, GenState a))
genState st@(State _ p) defEnv gen hOut =
  case fetchState st gen of
  Left stateName -> return $ Left stateName
  Right r@(stateName, _) -> do hPutStrLn hOut $ "  " ++ stateName ++ " // @pos(" ++ (show (procInfo p)) ++ ")"
                               return $ Right r

genStateTrans:: Show a => String -> State a -> DefEnv a -> GenState a -> Handle -> IO (GenState a)
genStateTrans stName st@(State _ p) defEnv gen hOut =
  case symStep p defEnv of
  Left (SymbolicError err) -> do hPutStrLn hOut ("  // Error (symbolic step): " ++ (show err))
                                 return gen -- abort graph creation
  Right syms ->
    case Set.toList (semTrans st syms) of
    [] -> return $ gen { fixedStates = (Map.insert st stName (fixedStates gen)) } -- register deadlock states
    allTrans -> genAllTrans stName allTrans defEnv gen hOut Map.empty

genAllTrans:: Show a => String -> [Transition a] -> DefEnv a -> GenState a -> Handle -> (Map (State a) String) -> IO (GenState a) 
genAllTrans _ [] defEnv gen hOut nextStates =
  genAllStateTrans (Map.toList nextStates) defEnv gen hOut

genAllTrans prevName ((Transition act nextSt):ts) defEnv gen hOut nextStates =
  do query <- case Map.lookup nextSt nextStates of
                   Nothing -> genState nextSt defEnv gen hOut
                   Just nextName -> return (Left nextName)
     let (nextName, gen', nextStates') = case query of
           Left nextName -> (nextName, gen, nextStates)
           Right (nextName, gen') -> (nextName, gen', (Map.insert nextSt nextName nextStates))
     hPutStrLn hOut ("  " ++ prevName ++ " -> " ++ nextName ++ " [label=\"" ++ (showAction act) ++ "\"];")
     genAllTrans prevName ts defEnv (gen' { nbTrans = (nbTrans gen) + 1 }) hOut nextStates'

genAllStateTrans:: Show a => [(State a, String)] -> DefEnv a -> GenState a -> Handle -> IO (GenState a)
genAllStateTrans [] _ gen _ = return gen
genAllStateTrans ((st,stName):sts) defEnv gen hOut =
  do gen' <- genStateTrans stName st defEnv gen hOut
     genAllStateTrans sts defEnv gen' hOut
