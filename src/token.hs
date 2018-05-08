module Token (Token(..)) where

import TokenType

data Token = Token
  { token_type :: TokenType
  , token_lexeme :: String
  , token_line :: Int
  } deriving (Show, Eq, Ord)
