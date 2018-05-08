module Instruction where

data Instruction = Instruction
  { address :: Int
  , operation :: String
  , operand :: Int
  } deriving (Show, Eq, Ord)
