module TokenType where

data TokenType = Identifier
               | Keyword
               | Int
               | Real
               | RParen
               | LParen
               | LBrace
               | RBrace
               | LBracket
               | RBracket
               | Colon
               | Semicolon
               | Comma
               | EndOfDefs
               | Plus
               | Minus
               | Times
               | Div
               | Greater
               | Less
               | EGT
               | ELT
               | Assign
               | Equals
               | NEquals
               | Whitespace
               | Newline
               | UnexpectedEOF
               | Unknown
               | EOF
               deriving (Show, Eq, Ord, Read)
