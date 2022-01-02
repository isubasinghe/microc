{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module MicroCodegen where

import Control.Monad.State
import Data.List (find)
import qualified Data.Map as M
import Data.String (fromString)
import Data.String.Conversions
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import LLVM.AST (Operand)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Name
import qualified LLVM.AST.Type as AST
import qualified LLVM.AST.Typed (typeOf)
import qualified LLVM.IRBuilder.Constant as L
import qualified LLVM.IRBuilder.Instruction as L
import qualified LLVM.IRBuilder.Module as L
import qualified LLVM.IRBuilder.Monad as L
import LLVM.Prelude (ShortByteString)
import MicroAST
  ( Bind (..),
    Op (..),
    Struct (..),
    Type (..),
    Uop (..),
  )
import MicroSAST

data Env = Env
  { operands :: M.Map Text Operand,
    structs :: [Struct],
    strings :: M.Map Text Operand
  }
  deriving (Show, Eq)

registerOperand :: MonadState Env m => Text -> Operand -> m ()
registerOperand name op = modify $ \env -> env {operands = M.insert name op (operands env)}

type LLVM = L.ModuleBuilderT (State Env)

type Codegen = L.IRBuilderT LLVM

getFields :: MonadState Env m => Text -> m [Bind]
getFields name = do
  ss <- gets structs
  case find (\s -> structName s == name) ss of
    Nothing -> error "Internal error - struct not found"
    Just (Struct _ binds) -> pure binds

charStar :: AST.Type
charStar = AST.ptr AST.i8

instance ConvertibleStrings Text ShortByteString where
  convertString = fromString . T.unpack

ltypeOfTyp :: MonadState Env m => Type -> m AST.Type
ltypeOfTyp = \case
  TyVoid -> pure AST.void
  TyInt -> pure AST.i32
  TyChar -> pure AST.i8
  TyFloat -> pure AST.double
  TyBool -> pure AST.i1
  Pointer TyVoid -> pure charStar
  Pointer (TyStruct n) ->
    pure $ AST.ptr (AST.NamedTypeReference (mkName $ cs ("struct." <> n)))
  Pointer t -> fmap AST.ptr (ltypeOfTyp t)
  TyStruct n -> do
    fields <- getFields n
    typs <- mapM (ltypeOfTyp . bindType) fields
    pure $ AST.StructureType {AST.isPacked = True, AST.elementTypes = typs}
sizeof :: MonadState Env m => Type -> m Word32 
sizeof = \case 
  TyBool -> pure 1 
  TyChar -> pure 1 
  TyInt -> pure 4 
  TyFloat -> pure 8 
  TyVoid -> pure 0 
  Pointer _ -> pure 8 
  TyStruct n -> do 
    fields <- getFields n 
    sizes <- mapM (sizeof . bindType) fields 
    pure (sum sizes)

codegenLVal :: LValue -> Codegen Operand 
codegenLVal (SId name) = gets ((M.! name) . operands)
codegenLVal (SDeref e) = codegenSexpr e 
codegenLVal (SAccess e i) = do 
  e' <- codegenLVal e 
  let zero = L.int32 0 
      offset = L.int32 (fromIntegral i)
  L.gep e' [zero, offset]

codegenSexpr :: SExpr -> Codegen Operand 
codegenSexpr = undefined



