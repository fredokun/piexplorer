
module Parser (parseProcessExpr, parseProcessDef, parseGraphExpr, parseFile
              , ParseResult (ParseExpr, ParseExprError, ParseDefDuplicateParamError
                            ,ParseDefError, ParseDef, ParseGraphExprError, ParseGraphExpr
                            , ParseFileError, ParseFile)
              , ASTInfo (ASTInfo) ) where


import Text.Parsec hiding (string)
import Text.Parsec.String (Parser)
import Text.Parsec.Language ()
import qualified Text.Parsec.Token as Tok

import Text.Parsec.Expr

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace

import Syntax


data ASTInfo = ASTInfo SourcePos SourcePos

showSourcePos:: SourcePos -> String
showSourcePos pos = (show (sourceLine pos)) ++ ":" ++ (show (sourceColumn pos))

instance Show ASTInfo where
  show (ASTInfo startP endP) = (showSourcePos startP) ++ "->" ++ (showSourcePos endP)

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = "/*"
  , Tok.commentEnd      = "*/"
  , Tok.commentLine     = "//"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> char '_'
  , Tok.opStart         = oneOf ""
  , Tok.opLetter        = oneOf ""
  , Tok.reservedNames   = [ "def", "end", "tau", "new"
                          ]
  , Tok.reservedOpNames = [ ",", ".", "?", "!", "=", "<>", "#" ]
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Tok.commaSep1 lexer

integer :: Parser Integer
integer = Tok.integer lexer

semi:: Parser ()
semi = do _ <- Tok.semi lexer
          return ()

graphExpr :: Parser (String, (Process ASTInfo))
graphExpr = do reserved "graph"
               fname <- identifier
               whiteSpace
               p <- procExpr
               return (fname, p)

procFile :: Parser [Def ASTInfo]
procFile = do defs <- many procDef
              eof
              return defs

procDef :: Parser (Def ASTInfo)
procDef = do whiteSpace
             startPos <- getPosition
             reserved "def"
             def <- identifier
             params <- parens $ commaSep identifier
             whiteSpace
             reservedOp "="
             whiteSpace
             body <- procExpr
             endPos <- getPosition
             return $ Def def params body (ASTInfo startPos endPos)

procExpr    = buildExpressionParser table procSimple
              <?> "process expression"

table   = [[binary "+" (mkBin Sum) AssocLeft]
          , [binary "||" (mkBin Par) AssocLeft]
          ]

binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc

mkBin cons p q = case (procInfo p, procInfo q) of
                   (ASTInfo startP _, ASTInfo _ endQ) -> cons p q (ASTInfo startP endQ)

procSimple    =  parens procExpr
                 <|> procEnd <|> procZero
                 <|> mmProcess
                 <|> brackets procExpr
                 <|> resProcess
                 <|> (try prefixProcess)
                 <|> callProcess
                 <?> "simple process expression"

procEnd:: Parser (Process ASTInfo)
procEnd = do startPos <- getPosition
             reserved "end"
             endPos <- getPosition
             return (Term (ASTInfo startPos endPos)) 

procZero:: Parser (Process ASTInfo)
procZero = do startPos <- getPosition
              n <- integer
              if n == 0 then do endPos <- getPosition
                                return $ Term (ASTInfo startPos endPos)
                else unexpected $ "integer '" ++ (show n) ++ "', do you mean '0' for termination?"


data Action = ActTau
            | ActOut Name Name
            | ActIn Name String

action:: Parser Action
action = tauAction <|> inOutAction <?> "action (tau, input, output)"

tauAction:: Parser Action
tauAction = do reserved "tau"
               return ActTau

inOutAction:: Parser Action
inOutAction = do chan <- identifier
                 isOut <- (do { (reservedOp "!") ; return True }
                           <|> do { (reservedOp "?") ; return False } )
                 if isOut then (outAction chan) else (inAction chan)

outAction:: String -> Parser Action
outAction chan = do datum <- identifier
                    return $ ActOut (Static chan) (Static datum)

inAction:: String -> Parser Action
inAction chan = do var <- parens identifier
                   return $ ActIn (Static chan) var

data MatchOp = MATCH | MISMATCH

mmProcess:: Parser (Process ASTInfo)
mmProcess = do startPos <- getPosition
               _ <- reservedOp "["
               a <- identifier
               matchOp <- (do { (reservedOp "=" ) ; return MATCH }
                           <|> do { (reservedOp "<>") ; return MISMATCH })
               b <- identifier
               _ <- reservedOp "]"
               p <- procSimple
               endPos <- getPosition
               return $ case matchOp of
                          MATCH -> (Match (Static a) (Static b) p (ASTInfo startPos endPos))
                          MISMATCH -> (Mismatch (Static a) (Static b) p (ASTInfo startPos endPos))

matchProcess:: Parser (Process ASTInfo)
matchProcess = do startPos <- getPosition
                  _ <- reservedOp "["
                  a <- identifier
                  _ <- reservedOp "="
                  b <- identifier
                  _ <- reservedOp "]"
                  p <- procSimple
                  endPos <- getPosition
                  return (Match (Static a) (Static b) p (ASTInfo startPos endPos))

mismatchProcess:: Parser (Process ASTInfo)
mismatchProcess = do startPos <- getPosition
                     _ <- reservedOp "["
                     a <- identifier
                     _ <- reservedOp "#"
                     b <- identifier
                     _ <- reservedOp "]"
                     p <- procSimple
                     endPos <- getPosition
                     return (Mismatch (Static a) (Static b) p (ASTInfo startPos endPos))

prefixProcess:: Parser (Process ASTInfo)
prefixProcess = do startPos <- getPosition
                   act <- action
                   p <- restProcess
                   endPos <- getPosition
                   case act of
                     ActTau -> return $ Step p (ASTInfo startPos endPos)
                     ActOut chan datum -> return $ Output chan datum p  (ASTInfo startPos endPos)
                     ActIn chan var -> return $ Input chan var p  (ASTInfo startPos endPos)

restProcess:: Parser (Process ASTInfo)
restProcess = do startP <- getPosition
                 do try (do (reservedOp "." <|> reservedOp ",")
                            q <- try prefixProcess
                                 <|>  procExpr
                            return q)
                    <|> return (Term  (ASTInfo startP startP))

resProcess:: Parser (Process ASTInfo)
resProcess = do startPos <- getPosition
                reserved "new"
                var <- parens identifier
                p <- procExpr
                endPos <- getPosition
                return (Restrict var p  (ASTInfo startPos endPos))

callProcess:: Parser (Process ASTInfo)
callProcess =
  do -- traceM "callProcess"
     startPos <- getPosition
     def <- identifier
     args <- parens (commaSep identifier)
     endPos <- getPosition
     let args' = map Static args
     return (Call def args' (ASTInfo startPos endPos))


data ParseResult a = ParseExprError ParseError
                   | ParseExpr (Process a)
                   | ParseDefError ParseError
                   | ParseDefDuplicateParamError (Def a) String
                   | ParseDef (Def a)
                   | ParseGraphExprError ParseError
                   | ParseGraphExpr (String, Process a)
                   | ParseFile (DefEnv a)
                   | ParseFileError ParseError

parseProcessExpr :: String -> String -> ParseResult ASTInfo
parseProcessExpr src s = case (parse procExpr src s) of
  Left err -> ParseExprError err
  Right p -> ParseExpr $ simplifyProc (encloseProc Set.empty p)

parseProcessDef :: String -> String -> ParseResult ASTInfo
parseProcessDef src s = case (parse procDef src s) of
  Left err -> ParseDefError err
  Right def@(Def name params p info) ->
    case prepareDef params p of
    Left param -> ParseDefDuplicateParamError def param
    Right p' -> ParseDef (Def name params p' info)

prepareDef:: [String] -> Process a -> Either String (Process a)
prepareDef params p =
  case prepare params Set.empty of
  Left param -> Left param
  Right bound -> Right (encloseProc bound p)
  where prepare:: [String] -> Set String -> Either String (Set String)
        prepare [] bound = Right bound
        prepare (p:ps) bound = if Set.member p bound
                               then Left p
                               else prepare ps (Set.insert p bound) 

parseGraphExpr :: String -> String -> ParseResult ASTInfo
parseGraphExpr src s = case (parse graphExpr src s) of
  Left err -> ParseGraphExprError err
  Right (fname, p) -> ParseGraphExpr (fname, simplifyProc (encloseProc Set.empty p))


parseFile :: DefEnv ASTInfo -> String -> String -> ParseResult ASTInfo
parseFile defEnv src content =
  case parse procFile src content of
  Left err -> ParseFileError err
  Right defs ->
    case prepareDefs defEnv defs of
    Left (def, param) -> ParseDefDuplicateParamError def param
    Right defEnv' -> ParseFile defEnv'

  where prepareDefs:: DefEnv ASTInfo -> [Def ASTInfo] -> Either (Def ASTInfo, String) (DefEnv ASTInfo)
        prepareDefs defEnv [] = Right defEnv
        prepareDefs defEnv (def@(Def name params p info):defs) =
          case prepareDef params p of
          Left param -> Left (def, param)
          Right p' -> prepareDefs (Map.insert name (Def name params p' info) defEnv) defs

