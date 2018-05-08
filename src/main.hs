module Main
  where

import System.IO

import Parser
import Lexer
import Token
import TokenType
import AST
import Data.Maybe

main = do
  file <- getLine
  src <- readFile file
  let tokens = lexer src
  tree <- parseIO tokens parseRat18S
  putStrLn ""
  -- putStrLn (show (fromJust tree))
