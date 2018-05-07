module AST where

import Token
import Lexer

newtype Ident = Ident Token deriving (Eq, Ord, Show)

data Empty = Empty
  deriving (Eq, Ord, Show)

data Rat18S
    = Rat18S OptFunctionDefinitions OptDeclarationList StatementList
  deriving (Eq, Ord, Show)

data OptFunctionDefinitions
    = OptFunctionDefinitions FunctionDefinitions
    | EmptyDefs Empty
  deriving (Eq, Ord, Show)

data FunctionDefinitions = FunctionDefinitions Function FDPrime
  deriving (Eq, Ord, Show)

data FDPrime
    = FDPrime FunctionDefinitions
    | EmptyFDPrime Empty
  deriving (Eq, Ord, Show)

data Function
    = Function Token OptParameterList OptDeclarationList Body
  deriving (Eq, Ord, Show)

data OptParameterList
    = OptParameterList ParameterList
    | EmptyParamList Empty
  deriving (Eq, Ord, Show)

data ParameterList = ParameterList Parameter PLPrime
  deriving (Eq, Ord, Show)

data PLPrime
    = PLPrime ParameterList
    | PLPrimeEmpty Empty
  deriving (Eq, Ord, Show)

data Parameter = Parameter1 IDs Qualifier
  deriving (Eq, Ord, Show)

data Qualifier = QualifierInt | QualifierBoolean | QualifierReal
  deriving (Eq, Ord, Show)

data Body = Body StatementList
  deriving (Eq, Ord, Show)

data OptDeclarationList
    = OptDeclarationList DeclarationList
    | EmptyDecs Empty
  deriving (Eq, Ord, Show)

data DeclarationList = DeclarationList Declaration DLPrime
  deriving (Eq, Ord, Show)

data DLPrime
    = DLPrime DeclarationList | DLPrimeEmpty Empty
  deriving (Eq, Ord, Show)

data Declaration = Declaration1 Qualifier IDs
  deriving (Eq, Ord, Show)

data IDs = IDs Token IDsPrime
  deriving (Eq, Ord, Show)

data IDsPrime = IDsPrime IDs | IDsPrimeEmpty Empty
  deriving (Eq, Ord, Show)

data StatementList = StatementList Statement SLPrime
  deriving (Eq, Ord, Show)

data SLPrime
    = SLPrime StatementList | SLPrimeEmpty Empty
  deriving (Eq, Ord, Show)

data Statement
    = StatementCompound Compound
    | StatementAssign Assign
    | StatementIf If
    | StatementReturn Return
    | StatementPrint Print
    | StatementScan Scan
    | StatementWhile While
  deriving (Eq, Ord, Show)

data Compound = Compound StatementList
  deriving (Eq, Ord, Show)

data Assign = Assign Token Expression
  deriving (Eq, Ord, Show)

data If
    = IfElse Condition Statement | IfElseIf Condition Statement Statement
  deriving (Eq, Ord, Show)

data Return = Return RPrime
  deriving (Eq, Ord, Show)

data RPrime = RPrime Empty | RPrimeExp Expression
  deriving (Eq, Ord, Show)

data Print = Print Expression
  deriving (Eq, Ord, Show)

data Scan = Scan IDs
  deriving (Eq, Ord, Show)

data While = While Condition Statement
  deriving (Eq, Ord, Show)

data Condition = Condition Expression Relop Expression
  deriving (Eq, Ord, Show)

data Relop = Relop Token
  deriving (Eq, Ord, Show)

data Expression = Expression Term EPrime
  deriving (Eq, Ord, Show)

data EPrime
    = EPrimePlus Term EPrime | EPrimeMinus Term EPrime | EPrime Empty
  deriving (Eq, Ord, Show)

data Term = Term Factor TermPrime
  deriving (Eq, Ord, Show)

data TermPrime
    = TermPrimeMult Factor TermPrime
    | TermPrimeDiv Factor TermPrime
    | TermPrime Empty
  deriving (Eq, Ord, Show)

data Factor = Factor1 Primary | FactorPrimary Primary
  deriving (Eq, Ord, Show)

data Primary
    = Id Ident
    | Integer Integer
    | Call Ident IDs
    | Expr Expression
    | Double Double
    | BoolTrue
    | BoolFalse
  deriving (Eq, Ord, Show)
