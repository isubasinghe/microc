{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MicroSemant where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.State
import Control.Monad.Except
import MicroAST
import MicroSAST

type Vars = M.Map (Text, VarKind) Type

type Funcs = M.Map Text Function

type Structs = [Struct]

data Env = Env
  { vars :: Vars,
    funcs :: Funcs,
    structs :: Structs
  }

type Semant = ExceptT SemantError (State Env)

checkBinds :: VarKind -> BindingLoc -> [Bind] -> Semant [Bind]
checkBinds kind loc binds = do
  forM binds $ \case
    Bind TyVoid name -> throwError $ IllegalBinding name Void kind loc

    Bind ty     name -> do
      vars <- gets vars
      when (M.member (name, kind) vars)
        $ throwError (IllegalBinding name Duplicate kind loc)
      modify $ \env -> env { vars = M.insert (name, kind) ty vars }
      pure $ Bind ty name

checkFields :: Struct -> Semant Struct
checkFields s@(Struct name fields) = do
  fields' <- foldM addField M.empty fields
  pure $ Struct name (M.elems fields') -- this doesn't preserve ordering
 where
  addField acc field@(Bind t name) = case t of
    TyVoid -> throwError $ IllegalBinding name Void StructField (S s)
    _      -> if M.member name acc
      then throwError (IllegalBinding name Duplicate StructField (S s))
      else pure $ M.insert name field acc

builtIns :: Funcs
builtIns = M.fromList $ map
  toFunc
  [ ("printf"  , [Pointer TyChar], TyVoid)
  , ("printbig", [TyInt]         , TyVoid)
  , ("malloc"  , [TyInt]         , Pointer TyVoid)
  , ("free"    , [Pointer TyVoid], TyVoid)
  ]
 where
  toFunc (name, tys, retty) =
    (name, Function retty name (map (`Bind` "dummy_var") tys) [] [])

checkExpr :: Expr -> Semant SExpr
checkExpr expr = case expr of
  Literal  i -> pure (TyInt, SLiteral i)
  FLit f -> pure (TyFloat, SFlit f)
  BoolLit  b -> pure (TyBool, SBoolLit b)
  CharLit  c -> pure (TyChar, SCharLit c)
  StrLit   s -> pure (Pointer TyChar, SStrLit s)
  Sizeof   t -> pure (TyInt, SSizeof t)
  Null       -> pure (Pointer TyVoid, SNull)
  Noexpr     -> pure (TyVoid, SNoexpr)
