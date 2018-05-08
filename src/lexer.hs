{-
Author: Justin Chin
Email: jmchin@csu.fullerton.edu
Course: CPSC323
Professor: Dr. Choi
Assignment: Project 1 -- Lexical Analyzer

Implements a simple lexical analyzer for the RAT18S language introduced in
CPSC323 at Cal State University, Fullerton, by Dr. Choi

RAT18S is a simple imperative language similar to C in structure and style.

Operators: +, -, *, /, >, <, =>, =<, ==, ^=, =
Separators: (, ), {, }, [, ], ;, :, ','

Identifiers: [A-Za-z]+[0-9A-Za-z]*\$?
Integers   : [0-9]+
Reals      : [0-9]+\.[0-9]+

This analyzer uses a table driven DFSA (deterministic finite state automaton)
to consume a stream of characters, categorizing them into tokens.

Limitations:

This project served two purposes, to familiarize myself with Haskell and to
practice implementing a finite state machine. Haskell was chosen for this project
because of its powerful pattern matching, and expressiveness. Not knowing any
Haskell prior to this proved a challenge, and the resulting code can definitely
be made clearer, and more powerful.

- This version of the lexical analyzer does not perform any error reporting.
  Because of Haskell's pure functional nature, rendering side effects takes a little
  more effort. Having read more into this, it seems that Haskell's monads
  allow a programmer to chain operations and carry "state" in some way. This requires
  more research.

- This version of the lexical analyzer does not recognize strings. It was not clear
  from the project documentation if RAT18S supported string literals. Since it was not
  explicitly stated, string literals are currently not supported. Adding them in
  should not be difficult though, with the pattern matching, just create some more
  states in the DFSA.
-}

module Lexer
  where

import Data.Char
import Text.Printf
import Token
import TokenType

-- function mapping a character to an operator
operator ::  Char -> TokenType
operator tt | tt == '+'  = Plus
            | tt == '-'  = Minus
            | tt == '*'  = Times
            | tt == '/'  = Div
            | tt == '>'  = Greater
            | tt == '<'  = Less

-- function mapping a character to a separator
separator :: Char -> TokenType
separator sep | sep == '(' = LParen
              | sep == ')' = RParen
              | sep == '{' = LBrace
              | sep == '}' = RBrace
              | sep == '[' = LBracket
              | sep == ']' = RBracket
              | sep == ':' = Colon
              | sep == ';' = Semicolon
              | sep == ',' = Comma

-- define some lists
operators  = "+-*/><"

separators = "(){}[]:;,"

keywords   = ["function","return",
             "int","boolean","real",
             "if","else","endif",
             "put","get","while",
             "true","false"]

-- Match identifiers against keyword list
kwLookup :: Int -> String -> Token
kwLookup line str
  | str `elem` keywords = Token { token_type = Keyword
                                , token_lexeme = str
                                , token_line = line }
  | otherwise = Token{ token_type = Identifier
                     , token_lexeme = str
                     , token_line = line }

lexer :: String -> [Token]
lexer input = lexer1 1 (input ++ " ")              {- concat whitespace at end of input
                                                    to prevent EOF from ending a token -}

-- hack to kind of add line numbers to tokens by passing it as an argument through the execution thread
-- should go back at some point and figure out how to encapsulate this process in a state monad for a
-- more idiomatic approach, but this will have to do more now since we need line numbers for error
-- reporting in the parser

lexer1 :: Int -> String -> [Token]                      -- recursive driving function for the lexer
lexer1 line [] = [Token EOF "EOF" line]                                  -- base case
lexer1 line input =
  let
    (token,remaining) = dfsa line 0 "" input          -- start machine in state 0
  in
    case token_type token of
      Whitespace -> lexer1 line remaining
      Newline -> lexer1 (line + 1) remaining
      _ -> token : lexer1 line remaining

{-
    From some state, build a string of characters from input
    until a token is found, returning a pair
-}
dfsa :: Int -> Integer -> String -> String -> (Token,String)
dfsa line state currTokStr []     = (Token { token_type = UnexpectedEOF
                                          , token_lexeme = currTokStr
                                          , token_line = line }, "")
dfsa line state currTokStr (c:cs) =
  let
    (nextState,   isConsumed)      = getNextState state c
    (nextTokStr,  remaining)       = nextStrings currTokStr c cs isConsumed
    (isAccepting, token)           = accepting nextState line nextTokStr
  in
    if isAccepting
    then (token, remaining)
    else dfsa line nextState nextTokStr remaining

nextStrings :: String -> Char -> String -> Bool -> (String,String)
nextStrings tokStr c remaining isConsumed
  | isConsumed     = (tokStr ++ [c], remaining)
  | not isConsumed = (tokStr       , c:remaining)           -- cons unconsumed char onto remaining

charToString :: Char -> String
charToString c = [c]

-- Define accepting states for the machine
accepting :: Integer -> Int -> String -> (Bool,Token)

accepting 2 line currTokStr  = (True, (kwLookup line currTokStr))       -- Identifiers/Keywords
accepting 3 line currTokStr  = (True, Token { token_type = Identifier
                                            , token_lexeme = currTokStr
                                            , token_line = line })

accepting 12 line currTokStr = (True, Token { token_type = Int
                                            , token_lexeme = show $ (read currTokStr :: Int)
                                            , token_line = line }) -- Integers/Reals
accepting 13 line currTokStr = (True, Token { token_type = Real
                                            , token_lexeme = show $ (read currTokStr :: Double)
                                            , token_line = line })

accepting 20 line (x:xs) = (True, Token { token_type = operator x
                                        , token_lexeme = charToString x
                                        , token_line = line })
accepting 22 line _      = (True, Token { token_type = NEquals
                                        , token_lexeme = "^="
                                        , token_line = line })
accepting 24 line _      = (True, Token { token_type = Equals
                                        , token_lexeme = "=="
                                        , token_line = line })
accepting 25 line _      = (True, Token { token_type = ELT
                                        , token_lexeme = "<="
                                        , token_line = line })
accepting 26 line _      = (True, Token { token_type = EGT
                                        , token_lexeme = "=>"
                                        , token_line = line })
accepting 27 line _      = (True, Token { token_type = Assign
                                        , token_lexeme = "="
                                        , token_line = line })

accepting 30 line (x:xs) = (True, Token { token_type = separator x
                                        , token_lexeme = charToString x
                                        , token_line = line }) -- Separator
accepting 32 line _      = (True, Token { token_type = EndOfDefs
                                        , token_lexeme = "%%"
                                        , token_line = line })

accepting 51 line _ = (True, Token { token_type = Whitespace
                                   , token_lexeme = ""
                                   , token_line = line })                        -- Comment, treat it like whitespace

accepting 97 line _ = (True, Token { token_type = Newline
                                   , token_lexeme = ""
                                   , token_line = line })                           -- Newline, TODO: implement and increment line counter
accepting 98 line _ = (True, Token { token_type = Whitespace
                                   , token_lexeme = ""
                                   , token_line = line })                        -- Whitespace

accepting 100 line currTokStr = (True, Token { token_type = Unknown
                                               , token_lexeme = currTokStr
                                               , token_line = line })
accepting _ line currTokStr   = (False, Token { token_type = Unknown
                                              , token_lexeme = currTokStr
                                              , token_line = line }) -- all other states are non-accepting

getNextState :: Integer -> Char -> (Integer,Bool) -- Deterministically run machine to next state
getNextState 0 c
  | c `elem` separators = (30, True)      -- separator
  | c `elem` operators  = (20, True)      -- singleton operators
  | c == '%'            = (31, True)      -- end of function definitions
  | c == '^'            = (21, True)      -- beginning not equals
  | c == '='            = (23, True)      -- beginning of rest of relop
  | c == '!'            = (50, True)      -- beginning of comment
  | isLetter c          =  (1, True)      -- in id/keyword
  | isDigit  c          = (10, True)      -- in number
  | c == '\n'           = (97, True)      -- newline, increment line counter
  | isSpace  c          = (98, True)      -- whitespace final state
  | otherwise           = (99, True)      -- error

-- Idents/Keywords
getNextState 1 c
  | c == '$'      = (3, True)    -- ends an identifier
  | isLetter c    = (1, True)    -- accept any number of letters
  | isDigit  c    = (4, True)    -- accept any number of digits
  | otherwise     = (2, False)   -- non-identifier character, do not consume

-- Digit in ident/keyword
getNextState 4 c
  | c == '$'      = (3, True)
  | isDigit c     = (4, True)
  | isLetter c    = (1, True)
  | otherwise     = (99, False)

-- Numbers
getNextState 10 c
  | isDigit c     = (10, True)   -- accept any number of digits
  | c == '.'      = (11, True)   -- floating point number
  | otherwise     = (12, False)  -- non-digit, do not consume

getNextState 11 c
  | isDigit c     = (11, True)   -- continue floating point number
  | otherwise     = (13, False)  -- non-digit, do not consume

-- Operators
getNextState 21 c
  | c == '='      = (22, True)   -- NEquals
  | otherwise     = (99, False)  -- Unknown character

getNextState 23 c
  | c == '='      = (24, True)   -- Equals
  | c == '<'      = (25, True)   -- ELT
  | c == '>'      = (26, True)   -- EGT
  | otherwise     = (27, False)  -- Assign

getNextState 31 c
  | c == '%'      = (32, True)
  | otherwise     = (100, False)

-- Comments
getNextState 50 c
  | c == '!'      = (51, True)   -- End of comment
  | otherwise     = (50, True)

getNextState 99 c
  | isSpace c     = (100, False)
  | otherwise     = (99, True)

getNextState _ _   = (99, True)  -- Error, catch-all patterns not matching those defined above

-- helper functions to print Tokens relying on pattern matching
showTokenType :: Token -> String
showTokenType token = show $ token_type token

showTokenLexeme :: Token -> String
showTokenLexeme token = token_lexeme token

showTokenLineNumber :: Token -> String
showTokenLineNumber token = (show $ token_line token)


prettyPrint :: [Token] -> IO ()
prettyPrint [] = printf ""
prettyPrint (t:ts) =
  let
    token = showTokenType t
    lexeme = showTokenLexeme t
    line = showTokenLineNumber t
  in
    do
      printf "%12s %12s %12s\n" token lexeme line
      prettyPrint ts

prettyPrint1 :: Token -> IO ()
prettyPrint1 t =
  let
    token = showTokenType t
    lexeme = showTokenLexeme t
    line = showTokenLineNumber t
  in
    do
      printf "%12s %12s %12s\n" token lexeme line
