module Parser
  where

import Debug.Trace

import Control.Monad
import Control.Conditional
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

import Data.Char
import System.IO

import Lexer

import Token
import TokenType
import AST
import Symbol
import Instruction

-- ADT describing the state of a parser
data ParserState = ParserState
  { tokens :: [Token]
  , current :: Token
  , next :: Token
  , logs :: [String]
  , errors :: [ParserError]
  , symbols :: [Symbol]
  , instructions :: [Instruction]
  , cur_instr :: Int
  , cur_sym :: Int
  , jumpstack :: [Int]
  } deriving (Show)

-- ADT describing Parser errors
-- Constructor: ParserError
-- Values: The token itself, and the associated error message
data ParserError = ParserError Token String
  deriving (Show)

-- Defines synonym for type signature on right hand side
-- essentially "Parser" is a direct replacement for "ExceptT ParserError (StateT ParserState Identity)"
-- this defines the monad stack
type Parser = ExceptT ParserError (StateT ParserState Identity)

type Lexeme = String

-- TODO: add check to see if symbol already exists in the table
addSymbol :: String -> Parser ()
addSymbol sym_type = do
  cur <- peek
  s <- get
  let
    sym_name = token_lexeme cur
    sym_loc = cur_sym s
    sym = (Symbol sym_name sym_type (cur_sym s))
  put s { symbols = sym : symbols s
        , cur_sym = sym_loc + 1
        }

-- given a token, if its name and type match a symbol in the symbol list
-- return its memory location as the value
getSymbolAddress :: Token -> ParserState -> Int
getSymbolAddress token s =
  let
    syms = symbols s
    match = filter (matchesSymbol token) syms
    loc = mem_location $ head match
  in
    if null match
    then error ("Undeclared symbol: " ++ token_lexeme token ++ " in line " ++ (show $ token_line token))
    else loc

matchesSymbol :: Token -> Symbol -> Bool
matchesSymbol token sym =
  let
    tlexeme = token_lexeme token
    ttype = token_type token
    slexeme = name sym
    stype = symbol_type sym
  in
    -- case (tlexeme == slexeme && (show ttype) == stype) of
    case (tlexeme == slexeme) of
      True -> True
      _ -> False

-- TODO: Add check against boolean arithmetic
addInstruction :: String -> Int -> Parser ()
addInstruction operation operand =
  case operation of
    "JUMPZ" -> do
      s <- get
      let
        instr_loc = cur_instr s
        instr = (Instruction instr_loc operation operand)
      put s { instructions = instr : instructions s
            , cur_instr = instr_loc + 1
            , jumpstack = instr_loc : jumpstack s
            }
    _ -> do
      s <- get
      let
        instr_loc = cur_instr s
        instr = (Instruction instr_loc operation operand)
      put s { instructions = instr : instructions s
            , cur_instr = instr_loc + 1
            }

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

-- replace ParserState with backpatched instruction operand
-- should run for any JUMPZ instruction
backPatch :: Int -> Int -> Parser ParserState
backPatch back cur = do
  s <- get
  let
    index = back - 1
    instrs = reverse $ instructions s
    op = operation (instrs !! index)
    new = (Instruction back op cur)
    newinstr = (reverse (replaceAtIndex index new instrs))
  put s { instructions = newinstr
        , jumpstack = tail $ jumpstack s}
  s <- get
  return s

parseIO :: [Token] -> Parser a -> IO (Maybe a)
parseIO tokens parser = either (\es -> mapM_ printError es >> return Nothing) (return . Just) (parse tokens parser)
  where
    printError (ParserError token msg) =
        putStrLn $ "[line " ++ ((show . token_line) token) ++ "] Error at "  ++ (if' (token_type token == EOF) ("end") (token_lexeme token)) ++  ": " ++ msg

-- Given a list of tokens and a Parser we either return a list of errors, or the abstract syntax tree
parse :: [Token] -> Parser a -> Either [ParserError] a
parse tokens parser = runParser (initializeState tokens) parser

-- Takes a ParserState and a Parser and returns either a list of ParserErrors or an abstract syntax tree
runParser :: ParserState -> Parser a -> Either [ParserError] a
runParser state p =
  let (results, finalState) = runIdentity $ runStateT (runExceptT p) state
  in
    if null $ errors finalState
    then either (\e -> Left [e]) (Right) results
    else Left $ (reverse . errors) finalState

printSymbolTable :: [Symbol] -> Parser()
printSymbolTable [] = traceM("\n")
printSymbolTable (x:xs) = do
  let
    n = name x
    stype = symbol_type x
    memloc = show (mem_location x)
  traceM(n ++ "\t" ++ stype ++ "\t" ++ memloc)
  printSymbolTable xs

printInstructionList :: [Instruction] -> Parser ()
printInstructionList [] = traceM("\n")
printInstructionList (x:xs) = do
  let
    addr = show (address x)
    op = operation x
    oprnd = if operand x == 0 then "" else show (operand x)
  traceM(addr ++ "\t" ++ op ++ "\t" ++ oprnd)
  printInstructionList xs

initializeState :: [Token] -> ParserState
initializeState tokens = ParserState tokens (head tokens) (head $ tail tokens) [] [] [] [] 1 2000 []

-- determines if a token in the token stream matches a TokenType and an associated Lexeme (String)
match :: TokenType -> Lexeme -> String -> Parser Bool
match ttype lexeme err = do
  state <- get
  let
    cur_type = token_type $ current state
    cur_lexeme = token_lexeme $ current state
  case (cur_type == ttype && cur_lexeme == lexeme) of
      True -> return True
      _ -> (peek >>= \t -> pError t err)

matchType :: TokenType -> Parser Bool
matchType ttype = do
  state <- get
  let
    cur_type = token_type $ current state
  case cur_type == ttype of
      True -> return True
      _ -> return False

-- if current token matches a TokenType and a Lexeme, advance the ParserState and return the token found
-- consume :: TokenType -> Lexeme -> String -> Parser Token
consume :: TokenType -> Lexeme -> String -> Parser Token
consume ttype lexeme err = do
  cur <- peek
  -- trace ("Token: " ++ (show $ token_type cur) ++ " \t " ++ "Lexeme: " ++ (show $ token_lexeme cur))
  ifM (match ttype lexeme err) (advance >> return cur) (peek >>= \t -> pError t err)

consumeType :: TokenType -> String -> Parser Token
consumeType ttype err = do
  cur <- peek
  -- trace ("Token: " ++ (show $ token_type cur) ++ " \t " ++ "Lexeme: " ++ (show $ token_lexeme cur))
  ifM (matchType ttype) (advance >> return cur) (peek >>= \t -> pError t err)

-- returns the current token in the stream
peek :: Parser Token
peek = do
  s <- get
  return (current s)

lookahead :: Parser Token
lookahead = do
  s <- get
  return (next s)

pError :: Token -> String -> Parser a
pError token message = throwError $ ParserError token message

-- advances the ParserState
-- advance `gets` the current ParserState, and uses it as an argument to the anonymous function
-- which takes the current state, and overwrites it with the resulting state of advancing
advance :: Parser ()
advance = get >>= \state -> do
  cur <- peek
  put state { tokens = tail $ tokens state
            , current = next state
            , logs = ("Token: "
                      ++ (show $ token_type cur)
                      ++ " \t "
                      ++ "Lexeme: "
                      ++ (show $ token_lexeme cur)) : logs state
            , next = head $ tail $ tail (tokens state) }


-- gets the current ParserState and overwrites prepends the new error to the error list
handleParseError :: ParserError -> Parser ()
handleParseError err = do
  state <- get
  put state { errors = err : errors state }

-- Entry point into the recursive descent
parseRat18S :: Parser Rat18S
parseRat18S = do
  defs <- parseOptFunctionDefs
  consume EndOfDefs "%%" "Expecting '%%' after function definitions."
  decs <- parseOptDeclarationList
  stmts <- parseStatementList
  s <- get
  printSymbolTable (reverse $ symbols s)
  printInstructionList (reverse $ instructions s)
  return (Rat18S defs decs stmts)

parseOptFunctionDefs :: Parser OptFunctionDefinitions
parseOptFunctionDefs = do
  cur <- peek
  case cur of
    Token Keyword "function" _ -> do
      defs <- parseFunctionDefs
      return (OptFunctionDefinitions defs)
    _ -> return (EmptyDefs Empty)

parseFunctionDefs :: Parser FunctionDefinitions
parseFunctionDefs = do
  def <- parseFunction
  defsprime <- parseFDPrime
  return (FunctionDefinitions def defsprime)

parseFunction :: Parser Function
parseFunction = do
  consume Keyword "function" "Expecting keyword 'function' in function definition."
  id <- parseIdentifier
  cur <- peek
  case cur of
    Token LBracket _ _ -> do
      consumeType LBracket "Expecting '[' before optional paramater list."
      params <- parseOptParameterList
      consumeType RBracket "Expecting ']' after optional parameter list."
      decs <- parseOptDeclarationList
      body <- parseBody
      return (Function id params decs body)
    _ -> do
      decs <- parseOptDeclarationList
      body <- parseBody
      return (Function id (EmptyParamList Empty) decs body)

parseFDPrime :: Parser FDPrime
parseFDPrime = do
  cur <- peek
  case cur of
    Token Keyword "function" _ -> do
      defs <- parseFunctionDefs
      return (FDPrime defs)
    _ -> do
      return (EmptyFDPrime Empty)

parseOptParameterList :: Parser OptParameterList
parseOptParameterList = do
  params <- parseParameterList
  return (OptParameterList params)

parseParameterList :: Parser ParameterList
parseParameterList = do
  param <- parseParameter
  paramprime <- parsePLPrime
  return (ParameterList param paramprime)

parseParameter :: Parser Parameter
parseParameter = do
  id <- parseID
  consumeType Colon "Expecting ':' between identifier and qualifier in parameter list."
  quals <- parseQualifier
  return (Parameter1 id quals)

parsePLPrime :: Parser PLPrime
parsePLPrime = do
  cur <- peek
  case token_type cur of
    Comma -> do
      consumeType Comma "Expecting ',' between paramaters"
      param <- parseParameterList
      return (PLPrime param)
    _ -> do
      return (PLPrimeEmpty Empty)

parseOptDeclarationList :: Parser OptDeclarationList
parseOptDeclarationList = do
  cur <- peek
  case cur of
    Token Keyword "int" _ -> do
      decs <- parseDeclarationList
      return (OptDeclarationList decs)
    Token Keyword "real" _ -> do
      decs <- parseDeclarationList
      return (OptDeclarationList decs)
    Token Keyword "boolean" _ -> do
      decs <- parseDeclarationList
      return (OptDeclarationList decs)
    _ -> do
      return (EmptyDecs Empty)

parseDeclarationList :: Parser DeclarationList
parseDeclarationList = do
  dec <- parseDeclaration
  decprime <- parseDLPrime
  return (DeclarationList dec decprime)

parseDeclaration :: Parser Declaration
parseDeclaration = do
  qual <- parseQualifier
  id <- parseID
  return (Declaration1 qual id)


parseDLPrime :: Parser DLPrime
parseDLPrime = do
  cur <- peek
  case cur of
    Token Semicolon _ _ -> do
      return (DLPrimeEmpty Empty)
    Token Keyword "int" _ -> do
      decs <- parseDeclarationList
      return (DLPrime decs)
    Token Keyword "boolean" _ -> do
      decs <- parseDeclarationList
      return (DLPrime decs)
    Token Keyword "real" _ -> do
      decs <- parseDeclarationList
      return (DLPrime decs)
    _ -> do
      return (DLPrimeEmpty Empty)

parseQualifier :: Parser Qualifier
parseQualifier = do
  cur <- peek
  sym <- lookahead
  let
    sym_lexeme = token_lexeme sym
  case cur of
    Token Keyword "int" _ -> do
      advance
      addSymbol "Int"
      return QualifierInt
    Token Keyword "boolean" _ -> do
      advance
      addSymbol "Boolean"
      return QualifierBoolean
    Token Keyword "real" _ -> do
      advance
      addSymbol "Real"
      return QualifierReal
    _ -> peek >>= \e -> pError e "Expecting one of int, boolean, real."

parseStatementList :: Parser StatementList
parseStatementList = do
  stmt <- parseStatement
  stmtprime <- parseSLPrime
  return (StatementList stmt stmtprime)

parseStatement :: Parser Statement
parseStatement = do
  cur <- peek
  case cur of
    Token LBrace _ _ -> do
      compound <- parseCompound
      return (StatementCompound compound)
    Token Keyword "if" _ -> do
      ifexpr <- parseIf
      return (StatementIf ifexpr)
    Token Keyword "while" _ -> do
      while <- parseWhile
      return (StatementWhile while)
    Token Keyword "get" _ -> do
      scan <- parseScan
      return (StatementScan scan)
    Token Keyword "put" _ -> do
      printexpr <- parsePrint
      return (StatementPrint printexpr)
    Token Keyword "return" _ -> do
      ret <- parseReturn
      return (StatementReturn ret)
    Token Identifier _ _ -> do
      assign <- parseAssign
      return (StatementAssign assign)
    _ -> peek >>= \t -> pError t "Unexpected token in statement"

parseSLPrime :: Parser SLPrime
parseSLPrime = do
  cur <- peek
  case cur of
    Token LBrace _ _ -> do
      stmtlst <- parseStatementList
      return (SLPrime stmtlst)
    Token Keyword "if" _ -> do
      stmtlst <- parseStatementList
      return (SLPrime stmtlst)
    Token Keyword "while" _ -> do
      stmtlst <- parseStatementList
      return (SLPrime stmtlst)
    Token Keyword "scan" _ -> do
      stmtlst <- parseStatementList
      return (SLPrime stmtlst)
    Token Keyword "put" _ -> do
      stmtlst <- parseStatementList
      return (SLPrime stmtlst)
    Token Keyword "get" _ -> do
      stmtlst <- parseStatementList
      return (SLPrime stmtlst)
    Token Keyword "return" _ -> do
      stmtlst <- parseStatementList
      return (SLPrime stmtlst)
    Token Identifier _ _ -> do
      stmtlst <- parseStatementList
      return (SLPrime stmtlst)
    _ -> do
      return (SLPrimeEmpty Empty)

parseID :: Parser IDs
parseID = do
  id <- parseIdentifier
  idprime <- parseIDPrime
  return (IDs id idprime)

parseIDPrime :: Parser IDsPrime
parseIDPrime = do
  cur <- peek
  case token_type cur of
    Comma -> do
      consume Comma "," "Expecting ','."
      id <- parseID
      return (IDsPrime id)
    Colon -> do
      return (IDsPrimeEmpty Empty)
    Semicolon -> do
      consume Semicolon ";" "Expecting ';'"
      return (IDsPrimeEmpty Empty)
    RParen -> do
      return (IDsPrimeEmpty Empty)
    _ -> (peek >>= \t -> pError t "Unexpected token in IDs.")

parseBody :: Parser Body
parseBody = do
  _ <- consume LBrace "{" "Expecting '{' before statement list."
  stmts <- parseStatementList
  _ <- consume RBrace "}" "Expecting '}' after statement list."
  return (Body stmts)

parseCondition :: Parser Condition
parseCondition = do
  expr <- parseExpression
  cur <- peek                -- save current token before parsing relop
  relop <- parseRelop
  expr2 <- parseExpression
  s <- get
  case token_type cur of
    Greater -> do
      addInstruction "GRT" 0
      addInstruction "JUMPZ" 0
    Less -> do
      addInstruction "LES" 0
      addInstruction "JUMPZ" 0
    EGT -> do
      addInstruction "GEQ" 0
      addInstruction "JUMPZ" 0
    ELT -> do
      addInstruction "LEQ" 0
      addInstruction "JUMPZ" 0
    Equals -> do
      addInstruction "EQU" 0
      addInstruction "JUMPZ" 0
    NEquals -> do
      addInstruction "NEQ" 0
      addInstruction "JUMPZ" 0
  return (Condition expr relop expr2)

parseRelop :: Parser Relop
parseRelop = do
  cur <- peek
  case cur of
    Token Greater _ _ -> do
      advance
      return (Relop cur)
    Token Less _ _ -> do
      advance
      return (Relop cur)
    Token EGT _ _ -> do
      advance
      return (Relop cur)
    Token ELT _ _ -> do
      advance
      return (Relop cur)
    Token Equals _ _ -> do
      advance
      return (Relop cur)
    Token NEquals _ _ -> do
      advance
      return (Relop cur)

parseExpression :: Parser Expression
parseExpression = do
  term <- parseTerm
  expprime <- parseExpressionPrime
  return (Expression term expprime)

parseExpressionPrime :: Parser EPrime
parseExpressionPrime = do
  cur <- peek
  case cur of
    Token Plus _ _ -> do
      consumeType Plus "Expecting '+' in expression."
      term <- parseTerm
      addInstruction "ADD" 0
      expprime <- parseExpressionPrime
      return (EPrimePlus term expprime)
    Token Minus _ _-> do
      consumeType Minus "Expecting '-' in expression."
      term <- parseTerm
      addInstruction "SUB" 0
      expprime <- parseExpressionPrime
      return (EPrimeMinus term expprime)
    _ -> do
      return (EPrime Empty)

parseTerm :: Parser Term
parseTerm = do
  fact <- parseFactor
  tprime <- parseTermPrime
  return (Term fact tprime)

parseTermPrime :: Parser TermPrime
parseTermPrime = do
  cur <- peek
  case cur of
    Token Times _ _ -> do
      consumeType Times "Expecting '*'."
      factor <- parseFactor
      addInstruction "MUL" 0
      tprime <- parseTermPrime
      return (TermPrimeMult factor tprime)
    Token Div _ _ -> do
      consumeType Div "Expecting '/'."
      factor <- parseFactor
      addInstruction "DIV" 0
      tprime <- parseTermPrime
      return (TermPrimeDiv factor tprime)
    _ -> do
      return (TermPrime Empty)

parseFactor :: Parser Factor
parseFactor = do
  prim <- parsePrimary
  return (FactorPrimary prim)

parsePrimary :: Parser Primary
parsePrimary = do
  cur <- peek
  next <- lookahead
  s <- get
  case token_type cur of
    LParen -> do
      consumeType LParen "Expecting '(' before expression."
      expr <- parseExpression
      consumeType RParen "Expecting ')' after expression."
      return (Expr expr)
    Identifier -> do
      let
        oprnd = getSymbolAddress cur s
      addInstruction "PUSHM" oprnd
      case token_type next of
        LParen -> do
          ident <- parseIdentifier
          consumeType LParen "Expecting '(' before function arguments."
          args <- parseID
          consumeType RParen "Expecting ')' after function arguments."
          return (Call (Ident ident) args)
        _ -> do
          advance
          return (Id (Ident cur))
    _ -> do
      cur <- peek
      advance
      case cur of
        Token Int n _ -> do
          addInstruction "PUSHI" (read n)
          return (Integer (read n))
        Token Real r _ -> do
          -- addInstruction "PUSHI" (read r)  -- no Real type in simplified Rat18S
          return (Double (read r))
        Token Keyword "true" _ -> do
          addInstruction "PUSHI" 1
          return (BoolTrue)
        Token Keyword "false" _ -> do
          addInstruction "PUSHI" 0
          return (BoolFalse)
        Token EOF _ _ -> peek >>= \t -> pError t "Unexpected end of file"

parseIdentifier :: Parser Token
parseIdentifier = do
  consumeType Identifier "parseIdentifier: Expecting identifier."

parseIf :: Parser If
parseIf = do
  consume Keyword "if" "Expecting keyword 'if' in If statement."
  consume LParen "(" "Expecting '(' in if-expression."
  s <- get
  let
    addr = cur_instr s      -- save current address to jump back to
  cond <- parseCondition
  consume RParen ")" "Expecting ')' in if-expression."
  stmt <- parseStatement
  s <- get
  backPatch (head $ jumpstack s) (cur_instr s)  -- fix ParserState with instr address to jump to
  next <- peek
  case next of
    Token Keyword "else" _ -> do
      consume Keyword "else" "Expecting keyword 'else' in If-Else statement"
      stmt2 <- parseStatement
      consume Keyword "endif" "Expecting keyword 'endif'."
      return (IfElseIf cond stmt stmt2)
    _ -> do
      consume Keyword "endif" "Expecting keyword 'endif'."
      return (IfElse cond stmt)

parseReturn :: Parser Return
parseReturn = do
  consume Keyword "return" "Expecting keyword 'return'."
  next <- peek
  case next of
    Token Semicolon _ _ -> do
      consumeType Semicolon "Expecting ';' at end of return statement"
      return (Return (RPrime Empty))
    _ -> do
      expr <- parseExpression
      consumeType Semicolon "Expecting ';' at end of return statement"
      return (Return $ RPrimeExp expr)

parsePrint :: Parser Print
parsePrint = do
  consume Keyword "put" "Expecting keyword 'put'."
  consume LParen "(" "Expecting '(' before expression."
  expr <- parseExpression
  consume RParen ")" "Expecting ')' at end of expression."
  consumeType Semicolon "Expecting ';' at end of print statement."
  addInstruction "STDOUT" 0
  return (Print expr)

parseScan :: Parser Scan
parseScan = do
  consume Keyword "get" "Expecting keyword 'get'."
  consume LParen "(" "Expecting '('"
  addInstruction "STDIN" 0
  cur <- peek
  ids <- parseID
  consume RParen ")" "Expecting ')'."
  consume Semicolon ";" "Expecting ';' at end of statement."
  s <- get
  addInstruction "POPM" (getSymbolAddress cur s)
  return (Scan ids)

parseWhile :: Parser While
parseWhile = do
  consume Keyword "while" "Expecting keyword while."
  s <- get
  let
    addr = cur_instr s      -- save current address to jump back to
  addInstruction "LABEL" 0
  consume LParen "(" "Expecting '('."
  cond <- parseCondition
  consume RParen ")" "Expecting ')'."
  stmt <- parseStatement
  addInstruction "JUMP" addr
  s <- get
  backPatch (head $ jumpstack s) (cur_instr s)
  return (While cond stmt)

parseCompound :: Parser Compound
parseCompound = do
  consume LBrace "{" "Expecting '{'."
  stmts <- parseStatementList
  consume RBrace "}" "Expecting '}'."
  return $ Compound stmts

parseAssign :: Parser Assign
parseAssign = do
  save <- peek
  ident <- parseIdentifier
  consume TokenType.Assign "=" "Expecting '='."
  expr <- parseExpression
  s <- get
  let
    oprnd = getSymbolAddress save s
  addInstruction "POPM" oprnd
  consume Semicolon ";" "Expecting ';'."
  return $ AST.Assign ident expr

parseEmpty :: Parser Empty
parseEmpty = return Empty
