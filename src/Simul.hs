
module Simul where

import System.IO
import Data.Char

import Data.Set (Set)
import qualified Data.Set as Set

import Utils

import Syntax
import Symbolic
import Semop


data SimCommand = QuitSim
                | ChooseTrans Int
                | SymbolicSteps
                | HelpSim String
                | SimCmdError String

buildSimCommand:: String -> SimCommand
buildSimCommand cmd
  | cmd == ":quit" || cmd == ":q" = QuitSim
  | cmd == ":step" = SymbolicSteps
  | cmd `startsWith` ":help" = HelpSim $ dropWhile isSpace (drop (length ":help") cmd)
  | cmd `startsWith` ":h" = HelpSim $ dropWhile isSpace (drop (length ":h") cmd)
  | not (cmd `startsWith` ":") = SimCmdError $ "Does not understand '" ++ cmd ++ "' (need :help ?)"
  | otherwise =
      case reads (drop 1 cmd) :: [(Int, String)] of
      [] -> SimCmdError $ "Don't know how to: " ++ cmd ++ " (need :help ?)"
      [(n, (_:_))] -> SimCmdError $ "Please keep repl out of garbage!"
      [(n, "")] -> ChooseTrans n

simulate:: State a -> DefEnv a -> IO ()
simulate s@(State _ p) defEnv =
  do putStrLn "----------------"
     putStrLn "Current state:"
     putStrLn (showState s)
     putStrLn "----------------"
     case symStep p defEnv of
       Left (SymbolicError err) ->
         do putStrLn $ "Error: " ++ err
            putStrLn "Aborpting simulation (sorry)."
            return ()
       Right syms -> let trans = Set.toList (semTrans s syms)
                     in do (case trans of
                            [] -> (putStrLn "<deadlock>")
                            _ -> do printTrans trans)
                           putStrLn "----------------"
                           simRepl s defEnv syms trans

simRepl:: State a -> DefEnv a -> Set (SymTrans a) -> [Transition a] -> IO ()
simRepl s@(State _ p) defEnv syms trans =
  do
     putStr "sim> "; hFlush stdout
     line <- getLine

     case buildSimCommand line of
       QuitSim -> do putStrLn "Quitting simulation mode (see'ya !)"
                     return ()

       SymbolicSteps -> do putStrLn "----------------"
                           putStrLn "Symbolic steps:"
                           putStrLn $ Set.foldr (\t s -> (show t) ++ "\n" ++ s) "" syms
                           simRepl s defEnv syms trans

       HelpSim thing -> do helpMeSim thing
                           simRepl s defEnv syms trans

       ChooseTrans nth -> if (nth >= 1) && (nth <= (length trans))
                          then do putStrLn "----------------"
                                  case (trans !! (nth - 1)) of
                                    t@(Transition _ s') -> do putStrLn (showTrans t)
                                                              simulate s' defEnv

                          else do putStrLn $ "No such transition: " ++ (show nth)
                                  simRepl s defEnv syms trans

       SimCmdError err -> do putStrLn $ "Error: " ++ err
                             simRepl s defEnv syms trans

printTrans:: [Transition a] -> IO ()
printTrans ts = print 1 ts
  where print _ [] = return ()
        print n (t@(Transition act s):ts) =
          do putStr $ ":" ++ (show n) ++ " => " ; putStrLn (showTrans t) ; hFlush stdout
             print (n+1) ts

helpSimCommand:: SimCommand -> String
helpSimCommand (HelpSim _) =
  ":help (:h) <thing>?\n\
  \      Need some information about <thing> ?"
helpSimCommand QuitSim =
  ":quit (:q)\n\
  \      Tired of simulating?"
helpSimCommand SymbolicSteps =
  ":step  \n\
  \       show the symbolic transitions from current state"
helpSimCommand (ChooseTrans _) =
  ":1 :2 ... :<num>\n\
  \       fire transition <num> from current state"
helpSimCommand (SimCmdError cmd) =
  "Cannot help you: no such command '" ++ (takeWhile (not . isSpace) cmd) ++ "...'"

helpMeSim "" = do putStrLn "Summary of commands:"
                  putStrLn "--------------------"
                  helpMeSim ":help"
                  helpMeSim ":quit"
                  helpMeSim ":step"
                  helpMeSim ":1"

helpMeSim thing
  | (thing == ":help") || (thing == ":h") || (thing == "help") = putStrLn $ helpSimCommand (HelpSim "")
  | (thing == ":quit") || (thing == ":q") || (thing == "quit") = putStrLn $ helpSimCommand QuitSim
  | (thing == ":step") || (thing == "step") = putStrLn $ helpSimCommand SymbolicSteps
  | (length thing) > 1 && (head thing) == ':' =
    case reads (drop 1 thing) :: [(Int, String)] of
    [] -> putStrLn $ helpSimCommand (SimCmdError thing)
    [(n, _)] -> putStrLn $ helpSimCommand (ChooseTrans 1)
  | otherwise = putStrLn $ helpSimCommand (SimCmdError thing)
