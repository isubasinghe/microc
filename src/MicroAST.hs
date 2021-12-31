module MicroAST where

import Data.Char (chr)
import Data.Text (Text)

data Op
  = Add
  | Sub
  | Mult
  | Div
  | Power
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | And
  | Or
  | BitAnd
  | BitOr
  deriving (Show, Eq)

data Uop
  = Neg
  | Not
  deriving (Show, Eq)

data Expr
  = Literal Int
  | StrLit Text
  | CharLit Int
  | FLit Double
  | BoolLit Bool
  | Null
  | Id Text
  | BinOp Op Expr Expr
  | Unop Uop Expr
  | Call Text [Expr]
  | Cast Type Expr
  | Access Expr Expr
  | Deref Expr
  | Addr Expr
  | Assign Expr Expr
  | Sizeof Type
  | Noexpr
  deriving (Show, Eq)

data Statement
  = Expr Expr 
  | Block [Statement]
  | Return Expr
  | If Expr Statement Statement
  | For Expr Expr Expr Statement
  | While Expr Statement
  deriving (Show, Eq)

data Type
  = Pointer Type
  | TyInt
  | TyBool
  | TyChar
  | TyFloat
  | TyVoid
  | TyStruct Text
  deriving (Show, Eq)

data Bind = Bind {bindType :: Type, bindName :: Text}
  deriving (Show, Eq)

data Struct = Struct {structName :: Text, structFields :: [Bind]}
  deriving (Show, Eq)

data Function = Function 
  { ty :: Type 
  , name :: Text 
  , formals :: [Bind]
  , locals :: [Bind]
  , body :: [Statement]}
  deriving (Show, Eq)

data Program = Program [Struct] [Bind] [Function]
  deriving (Show, Eq)
