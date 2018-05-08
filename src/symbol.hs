module Symbol (Symbol(..)) where

data Symbol = Symbol
  { name :: String
  , symbol_type :: String
  , mem_location :: Int
  } deriving (Show, Eq, Ord)
