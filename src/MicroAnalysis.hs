{-# LANGUAGE LambdaCase #-}
module MicroAnalysis where 

import MicroSAST

data CFG = Empty | Seq Bool CFG | Branch CFG CFG 
  deriving (Show, Eq)

genCFG :: [SStatement] -> CFG
genCFG [] = Empty
genCFG (s:ss) = case s of
    SReturn _ -> Seq True (genCFG ss)
    SIf _ cons alt -> Branch (genCFG (cons : ss)) (genCFG (alt:ss))
    SDoWhile _ stmt -> Seq False (genCFG (stmt:ss))
    SBlock stmts -> genCFG (stmts <> ss)
    _ -> Seq False (genCFG ss)

validate :: CFG -> Bool
validate = \case
  Empty -> False
  Seq b Empty -> b
  Seq _ rest -> validate rest
  Branch left right -> validate left && validate right
