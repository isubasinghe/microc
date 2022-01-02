module MicroSAST where

import Data.Text (Text)
import MicroAST

type SExpr = (Type, SExpr')

data SExpr'
  = SLiteral Int
  | SFlit Double
  | SStrLit Text
  | SCharLit Int
  | SBoolLit Bool
  | SNull
  | SBinOp Op SExpr SExpr
  | SUnOp Uop SExpr
  | SCall Text [SExpr]
  | SCast Type SExpr
  | LVal LValue
  | SAssign LValue SExpr
  | SAddr LValue
  | SSizeof Type
  | SNoexpr
  deriving (Show, Eq)

data LValue
  = SDeref SExpr
  | SAccess LValue Int
  | SId Text
  deriving (Show, Eq)

data SStatement
  = SExpr SExpr
  | SBlock [SStatement]
  | SReturn SExpr
  | SIf SExpr SStatement SStatement
  | SDoWhile SExpr SStatement
  deriving (Show, Eq)

data SFunction = SFunction
  { sty :: Type,
    sname :: Text,
    sformals :: [Bind],
    slocals :: [Bind],
    sbody :: SStatement
  }
  deriving (Show, Eq)

type SProgram = ([Struct], [Bind], [SFunction])

type Name = Text

data BindingLoc = F Function | S Struct | TopLevel
  deriving (Show, Eq)

data SemantError
  = IllegalBinding Name BindingKind VarKind BindingLoc
  | UndefinedSymbol Name SymbolKind Expr
  | TypeError {expected :: [Type], got :: Type, errorLoc :: Statement}
  | CastError {to :: Type, from :: Type, castLoc :: Statement}
  | ArgError {nExpected :: Int, nGot :: Int, callSite :: Expr}
  | Redeclaration Name
  | NoMain
  | AddressError Expr
  | AssignmentError {lhs :: Expr, rhs :: Expr}
  | AccessError {struct :: Expr, field :: Expr}
  | DeadCode Statement
  deriving (Show, Eq)

data BindingKind = Duplicate | Void deriving (Show, Eq)

data SymbolKind = Var | Func deriving (Show, Eq)

data VarKind = Global | Formal | Local | StructField
  deriving (Show, Eq, Ord)
