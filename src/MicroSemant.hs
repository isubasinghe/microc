{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module MicroSemant where

import Control.Monad.Except
import Control.Monad.State
import Data.List (find, findIndex)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
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
  deriving (Show, Eq)

type Semant = ExceptT SemantError (State Env)

checkBinds :: VarKind -> BindingLoc -> [Bind] -> Semant [Bind]
checkBinds kind loc binds = do
  forM binds $ \case
    Bind TyVoid name -> throwError $ IllegalBinding name Void kind loc
    Bind ty name -> do
      vars <- gets vars
      when (M.member (name, kind) vars) $
        throwError (IllegalBinding name Duplicate kind loc)
      modify $ \env -> env {vars = M.insert (name, kind) ty vars}
      pure $ Bind ty name

checkFields :: Struct -> Semant Struct
checkFields s@(Struct name fields) = do
  fields' <- foldM addField M.empty fields
  pure $ Struct name (M.elems fields') -- this doesn't preserve ordering
  where
    addField acc field@(Bind t name) = case t of
      TyVoid -> throwError $ IllegalBinding name Void StructField (S s)
      _ ->
        if M.member name acc
          then throwError (IllegalBinding name Duplicate StructField (S s))
          else pure $ M.insert name field acc

builtIns :: Funcs
builtIns =
  M.fromList $
    map
      toFunc
      [ ("printf", [Pointer TyChar], TyVoid),
        ("printbig", [TyInt], TyVoid),
        ("malloc", [TyInt], Pointer TyVoid),
        ("free", [Pointer TyVoid], TyVoid)
      ]
  where
    toFunc (name, tys, retty) =
      (name, Function retty name (map (`Bind` "dummy_var") tys) [] [])

isNumeric = \case
  TyInt -> True
  TyFloat -> True
  TyChar -> True
  Pointer _ -> True
  _ -> False

checkExpr :: Expr -> Semant SExpr
checkExpr expr = case expr of
  Literal i -> pure (TyInt, SLiteral i)
  FLit f -> pure (TyFloat, SFlit f)
  BoolLit b -> pure (TyBool, SBoolLit b)
  CharLit c -> pure (TyChar, SCharLit c)
  StrLit s -> pure (Pointer TyChar, SStrLit s)
  Sizeof t -> pure (TyInt, SSizeof t)
  Null -> pure (Pointer TyVoid, SNull)
  Noexpr -> pure (TyVoid, SNoexpr)
  Id s -> do
    vars <- gets vars
    let foundVars =
          map
            (\kind -> M.lookup (s, kind) vars)
            [Local, Formal, Global]
    case join $ find isJust foundVars of
      Nothing -> throwError $ UndefinedSymbol s Var expr
      Just ty -> pure (ty, LVal $ SId s)
  BinOp op lhs rhs -> do
    lhs'@(t1, _) <- checkExpr lhs
    rhs'@(t2, _) <- checkExpr rhs

    let assertSym = unless (t1 == t2) $ throwError $ TypeError [t1] t2 (Expr expr)

        checkArith = do
          unless (isNumeric t1) $
            throwError $
              TypeError [TyInt, TyFloat] t1 (Expr expr)
          pure (t1, SBinOp op lhs' rhs')

        checkBool = do
          unless (t1 == TyBool) $
            throwError $
              TypeError [TyBool] t1 (Expr expr)
          pure (t1, SBinOp op lhs' rhs')

    case op of
      Add ->
        let sexpr = SBinOp Add lhs' rhs'
         in case (t1, t2) of
              (Pointer t, TyInt) -> pure (Pointer t, sexpr)
              (TyInt, Pointer t) -> pure (Pointer t, sexpr)
              (TyInt, TyInt) -> pure (TyInt, sexpr)
              (TyFloat, TyFloat) -> pure (TyFloat, sexpr)
              _ -> throwError $ TypeError [Pointer TyVoid, TyInt, TyFloat] t1 (Expr expr)
      Sub ->
        let sexpr = SBinOp Sub lhs' rhs'
         in case (t1, t2) of
              (Pointer t, TyInt) -> pure (Pointer t, sexpr)
              (TyInt, Pointer t) -> pure (Pointer t, sexpr)
              (TyInt, TyInt) -> pure (TyInt, sexpr)
              (TyFloat, TyFloat) -> pure (TyFloat, sexpr)
              (Pointer t, Pointer t') -> if t == t' then pure (TyInt, sexpr) else throwError $ TypeError [Pointer t'] (Pointer t) (Expr expr)
              _ -> throwError $ TypeError [Pointer TyVoid, TyInt, TyFloat] t1 (Expr expr)
      Mult -> assertSym >> checkArith
      Div -> assertSym >> checkArith
      BitAnd -> assertSym >> checkArith
      BitOr -> assertSym >> checkArith
      And -> assertSym >> checkBool
      Or -> assertSym >> checkBool
      Power -> case (t1, t2) of
        (TyFloat, TyFloat) ->
          pure (TyFloat, SCall "llvm.pow.f64" [lhs', rhs'])
        (TyFloat, TyInt) ->
          pure (TyFloat, SCall "llvm.powi.i32" [lhs', rhs'])
        (TyInt, TyInt) ->
          pure (TyInt, SBinOp Power lhs' rhs')
        _ -> throwError $ TypeError [TyFloat, TyInt] t1 (Expr expr)
      relational -> case (snd lhs', snd rhs') of
        (SNull, _) -> checkExpr (BinOp relational (Cast t1 lhs) rhs)
        (_, SNull) -> checkExpr (BinOp relational lhs (Cast t1 rhs))
        _ -> do
          assertSym
          unless (isNumeric t1) $
            throwError (TypeError [TyInt, TyFloat] t1 (Expr expr))
          pure (TyBool, SBinOp op lhs' rhs')
  Unop op e -> do
    e'@(ty, _) <- checkExpr e
    case op of
      Neg -> do
        unless (isNumeric ty) $
          throwError $ TypeError [TyInt, TyFloat] ty (Expr expr)
        pure (ty, SUnOp Neg e')
      Not -> do
        unless (ty == TyBool) $
          throwError $
            TypeError [TyBool] ty (Expr expr)
        pure (ty, SUnOp Not e')
  Addr e -> do
    (t, e') <- checkExpr e 
    case e' of 
      LVal l -> pure (Pointer t, SAddr l)
      _ -> throwError $ AddressError e 
  Deref e -> do
    (ty, e') <- checkExpr e
    case ty of 
      Pointer t -> pure (t, LVal $ SDeref (ty, e'))
      _ -> throwError $ TypeError [Pointer TyVoid, Pointer TyInt, Pointer TyFloat] ty (Expr expr)
  Call "printf" es -> do
    es' <- mapM checkExpr es
    let (formatStr, _) = head es'
    unless (formatStr == Pointer TyChar) $
      throwError $ TypeError [Pointer TyChar] formatStr (Expr expr)
    pure (TyVoid, SCall "printf" es')
  Call s es -> do
    funcs <- gets funcs 
    case M.lookup s funcs of 
      Nothing -> throwError $ UndefinedSymbol $ Func expr 
      Just f -> do 
        es' <- mapM checkExpr es 
        let nFormals = length (formals f)
            nActuals = length es 
        unless (nFormals == nActuals) $ throwError $ ArgError nFormals nActuals expr 
  Cast t' e -> do
    undefined
  Access e field -> do
    undefined
  _ -> undefined
