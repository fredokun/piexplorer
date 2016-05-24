

import System.IO
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Utils
import Syntax
import Parser
import Symbolic
import Semop

import Simul

import GenGraph

import Debug.Trace

banner =  "                                                       \n"
       ++ "   ,############;'                                     \n"
       ++ "   ;#''##''##'''      =|=|_=  =|    =|  =|=|=|  =|=|   \n"
       ++ "   '   ##  ##       =|=|      =|    =|  =|    =|    =| \n"
       ++ "       ##  ##  :::      =|=|  =|    =|  =|    =|    =| \n"
       ++ "       ##  ##       =|=|=|      =|=|=|  =|    =|    =| \n"
       ++ "       ;'  ;'                       =|                 \n"
       ++ "                                =|=|                   \n"

main = do
  putStr banner
  putStrLn "------------------------------------------------------"
  putStrLn "pisym -- Symbolic analyzer for the Pi-calculus (v0.1)"
  putStrLn "(C) 2015-\\infty F. Peschanski"
  putStrLn "------------------------------------------------------"
  repl Single (Map.empty)

data Command = Quit
             | MultiMode
             | MakeDef String
             | PrintDef String
             | PrintDefAll
             | ParseProcExpr String
             | Help String
             | SymStep String
             | Simulator String
             | GenGraph String
             | LoadFile String
             | CmdError String

helpCommand:: Command -> String
helpCommand (Help _) =
  ":help (:h) <thing>?\n\
  \      Need some information about <thing> ?"
helpCommand Quit =
  ":quit (:q)\n\
  \      Tired of pisym?"
helpCommand MultiMode =
  ":multi (:m)\n\
  \      Enter multi-line input mode\n\
  \      (validate with 2 successive blank lines)"
helpCommand (ParseProcExpr _) =
  ":parse (:p) <process>\n\
  \      Parse a process expression"
helpCommand (MakeDef _) =
  ":def (def) <name>(<params>) = <process>\n\
  \      Register a new process definition <name>"
helpCommand (PrintDef _) =
  ":printdef (:pdef) <name>\n\
  \      Print a registered process definition <name>"
helpCommand PrintDefAll =
  ":printdefs (:pdefs)\n\
  \      Print all registered process definitions"
helpCommand (LoadFile _) =
  ":load (:l) <file>\n\
  \      Load definitions from <file>"
helpCommand (SymStep _) =
  ":step  <process>\n\
  \       show the symbolic transitions of <process>"
helpCommand (Simulator _) =
  ":simulate (:simul :s)  <process>\n\
  \       start interactive simulator for <process>"
helpCommand (GenGraph _) =
  ":graph (:lts :gen :g) <filename> <process>\n\
  \       generate stategraph for <process> in <file>.grf"
helpCommand (CmdError cmd) =
  "Cannot help you: no such command '" ++ (takeWhile (not . isSpace) cmd) ++ "...'"

helpMe "" = do putStrLn "Summary of commands:"
               putStrLn "--------------------"
               helpMe ":help"
               helpMe ":quit"
               helpMe ":multi"
               helpMe ":parse"
               helpMe ":def"
               helpMe ":pdef"
               helpMe ":pdefs"
               helpMe ":load"
               helpMe ":step"
               helpMe ":simul"
               helpMe ":graph"

helpMe thing
  | (thing == ":help") || (thing == ":h") || (thing == "help") = putStrLn $ helpCommand (Help "")
  | (thing == ":quit") || (thing == ":q") || (thing == "quit") = putStrLn $ helpCommand Quit
  | (thing == ":multi") || (thing == ":m") || (thing == "multi") = putStrLn $ helpCommand MultiMode
  | (thing == ":parse") || (thing == ":p") || (thing == "parse") = putStrLn $ helpCommand (ParseProcExpr "")
  | (thing == ":def") || (thing == "def") = putStrLn $ helpCommand (MakeDef "")
  | (thing == ":load") || (thing == ":l") || (thing == "load") = putStrLn $ helpCommand (LoadFile "")
  | (thing == ":printdef") || (thing == ":pdef")
    || (thing == "printdef") || (thing == "pdef") = putStrLn $ helpCommand (PrintDef "")
  | (thing == ":printdefs") || (thing == ":pdefs")
    || (thing == "printdefs") || (thing == "pdefs") = putStrLn $ helpCommand PrintDefAll
  | (thing == ":step") || (thing == "step") = putStrLn $ helpCommand (SymStep "")
  | (thing == ":simulate") || (thing == ":simul") || (thing == ":sim") || (thing == ":s")
    || (thing == "simulate") || (thing == "simul") || (thing == "sim") = putStrLn $ helpCommand (Simulator "")
  | (thing == ":graph") || (thing == ":gen") || (thing == ":g") || (thing == ":lts")
    || (thing == "graph") || (thing == "gen") || (thing == "lts") || (thing == "g") = putStrLn $ helpCommand (GenGraph "")
  | otherwise = putStrLn $ helpCommand (CmdError thing)

buildCommand:: String -> Command
buildCommand cmd
  | (cmd == ":quit") || (cmd == ":q") = Quit
  | (cmd == ":multi") || (cmd == ":m") = MultiMode
  | cmd `startsWith` ":help" = Help (dropWhile isSpace (drop 5 cmd))
  | cmd `startsWith` ":h" = Help (dropWhile isSpace (drop 2 cmd))
  | cmd `startsWith` ":parse " = ParseProcExpr (dropWhile isSpace (drop 6 cmd))
  | cmd == ":printdefs" = PrintDefAll
  | cmd == ":pdefs " = PrintDefAll
  | cmd `startsWith` ":printdef " = PrintDef (dropWhile isSpace (drop 10 cmd))
  | cmd `startsWith` ":pdef " = PrintDef (dropWhile isSpace (drop 5 cmd))
  | cmd `startsWith` ":def " = MakeDef (drop 1 cmd)
  | cmd `startsWith` ":load " = LoadFile $ (dropWhile isSpace (drop 5 cmd))
  | cmd `startsWith` "def " = MakeDef cmd
  | cmd `startsWith` ":step " = SymStep (dropWhile isSpace (drop 5 cmd))
  | cmd `startsWith` ":simulate " = Simulator (dropWhile isSpace (drop (length ":simulate") cmd))
  | cmd `startsWith` ":simul " = Simulator (dropWhile isSpace (drop (length ":simul") cmd))
  | cmd `startsWith` ":sim " = Simulator (dropWhile isSpace (drop (length ":sim") cmd))
  | cmd `startsWith` ":graph " = GenGraph $ "graph " ++ (dropWhile isSpace (drop (length ":graph") cmd))
  | cmd `startsWith` ":gen " = GenGraph $ "graph " ++ (dropWhile isSpace (drop (length ":gen") cmd))
  | cmd `startsWith` ":lts " = GenGraph $ "graph " ++ (dropWhile isSpace (drop (length ":lts") cmd))
  | cmd `startsWith` ":s " = Simulator (dropWhile isSpace (drop 2 cmd))
  | cmd `startsWith` ":p " = ParseProcExpr (dropWhile isSpace (drop 2 cmd))
  | cmd `startsWith` ":g " = GenGraph $ "graph " ++ (dropWhile isSpace (drop 2 cmd))
  | cmd `startsWith` ":l " = LoadFile $ (dropWhile isSpace (drop 2 cmd))
  | otherwise = CmdError cmd

data InputMode = Single | Multi

repl:: InputMode -> (DefEnv ASTInfo) -> IO ()
repl inputMode defEnv = do
  putStrLn ""
  putStr "> " ; hFlush stdout
  cmd <- readInput inputMode

  case buildCommand cmd of
    Quit -> byebye

    MultiMode -> do putStrLn "<Multiline mode: press 'Return' twice to validate>"
                    repl Multi defEnv

    CmdError cmd -> do putStrLn $ "Error: don't know how to '" ++ (takeWhile (not . isSpace) cmd) ++ "...'"
                       repl Single defEnv

    ParseProcExpr expr -> do -- putStrLn $ "Need to parse: '" ++ expr ++ "'"
                         doParseExpr "<repl>" expr
                         repl Single defEnv

    SymStep expr -> do doSymStepExpr "<repl>" defEnv expr
                       repl Single defEnv

    MakeDef expr -> doMakeDef Single defEnv expr

    PrintDef defName -> do doPrintDef defName defEnv
                           repl Single defEnv

    PrintDefAll -> do doPrintDefAll defEnv
                      repl Single defEnv

    Simulator expr -> do doSimulator "<repl>" expr defEnv
                         repl Single defEnv

    GenGraph expr -> do doGenGraph "<repl>" expr defEnv
                        repl Single defEnv

    LoadFile fname -> do doLoadFile Single defEnv fname

    Help thing -> do helpMe thing
                     repl Single defEnv


doSymStepExpr:: String -> (DefEnv ASTInfo) -> String -> IO ()
doSymStepExpr src defEnv expr =
  case parseProcessExpr "<repl>" expr of
  ParseExprError err -> do putStr "ParseError: "
                           putStrLn (show err)

  ParseExpr p ->
    case symStep p defEnv of
    Left (SymbolicError msg) -> putStrLn ("Error: " ++ msg)
    Right ts ->
      do 
        putStrLn "----------------"
        putStrLn $ Set.foldr (\t s -> (show t) ++ "\n" ++ s) "" ts

doSimulator:: String -> String -> DefEnv ASTInfo -> IO ()
doSimulator src expr defEnv =
  case parseProcessExpr src expr of
  ParseExprError err -> do putStr "ParseError: "
                           putStrLn (show err)
  ParseExpr p -> do putStrLn "----------------"
                    putStrLn "Entering simulator..."
                    simulate (initState p) defEnv

doParseExpr:: String -> String -> IO ()
doParseExpr src expr =
  case parseProcessExpr "<repl>" expr of
  ParseExprError err -> do putStr "Parse error: "
                           putStrLn (show err)

  ParseExpr p -> do putStrLn "----------------"
                    putStrLn (show p)

doMakeDef:: InputMode -> (DefEnv ASTInfo) -> String -> IO ()
doMakeDef inputMode defEnv defExpr =
  case parseProcessDef "<repl>" defExpr of
  ParseDefError err -> do putStr "Parse error: "
                          putStrLn (show err)
                          repl inputMode defEnv

  ParseDefDuplicateParamError (Def name _ _ _ ) param ->
    do putStrLn $ "Definition '" ++ name ++ "' error: duplicate parameter '" ++ param ++ "'"
       repl inputMode defEnv

  ParseDef def@(Def name _ _ _) -> 
    do putStrLn "----------------"
       let defEnv' = Map.insert name def defEnv
       putStr $ "Definition '" ++ name ++ "' registered"
       putStrLn (if Map.member name defEnv then " (previous definition overwritten)" else "")
       repl inputMode defEnv'

doPrintDef:: String -> DefEnv ASTInfo -> IO ()
doPrintDef defName defEnv =
  case Map.lookup defName defEnv of
  Nothing -> putStrLn $ "Error: no such definition '" ++ defName ++ "'"
  Just def -> do putStrLn "----------------"
                 putStrLn (show def)


doPrintDefAll:: DefEnv ASTInfo -> IO ()
doPrintDefAll defEnv = do putStrLn "----------------"
                          sequence_ $ map printOne (Map.toList defEnv)
  where --printOne:: Def ASTInfo -> IO ()
        printOne (_, def@(Def name _ _ info)) =
          do --putStrLn $ "Definition '" ++ name ++ "' at: " ++ (show info)
             putStrLn (show def)


doGenGraph:: String -> String -> DefEnv ASTInfo -> IO ()
doGenGraph src expr defEnv =
  case parseGraphExpr "<repl>" expr of
  ParseGraphExprError err -> do putStr "Parse error: "
                                putStrLn (show err)

  ParseGraphExpr (fname, p) -> do putStrLn "----------------"
                                  putStrLn $ "Generating LTS graph file '" ++ fname ++ ".lts' for:"
                                  putStrLn (show p)
                                  putStrLn "----------------"
                                  withFile (fname ++ ".lts") WriteMode (genGraph p defEnv src)

doLoadFile:: InputMode -> (DefEnv ASTInfo) -> String -> IO ()
doLoadFile inputMode defEnv fname =
  (do defEnv' <- withFile fname ReadMode (loadFile defEnv)
      repl inputMode defEnv')
  where loadFile:: DefEnv ASTInfo -> Handle -> IO (DefEnv ASTInfo)
        loadFile defEnv hfile =
          do content <- hGetContents hfile
             case parseFile defEnv fname content of
               ParseFileError err -> do putStr "Parse error: "
                                        putStrLn (show err)
                                        return defEnv

               ParseDefDuplicateParamError (Def name _ _ _ ) param ->
                 do putStrLn $ "Definition '" ++ name ++ "' error: duplicate parameter '" ++ param ++ "'"
                    return defEnv

               ParseFile defEnv' ->
                 do putStrLn "----------------"
                    putStrLn $ "File loaded"
                    return defEnv'


byebye = do putStrLn "Bye bye!"
            return ()

readInput:: InputMode -> IO String
readInput Single = getLine
readInput Multi = consumeLines 0 2

consumeLines:: Int -> Int -> IO String
consumeLines nbEmpty needEmpty =
  if nbEmpty == needEmpty then return ""
  else do line <- getLine
          -- _ <- trace ("Line read = \"" ++ line ++ "\" (consumed = " ++ (show nbEmpty) ++ ")") (return ())
          case line of
            "" -> consumeLines (nbEmpty + 1) needEmpty
            _ -> do lines <- consumeLines 0 needEmpty
                    return $ line ++ lines




