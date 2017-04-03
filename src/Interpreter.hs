module Interpreter where

import AbsStarsepLang
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transExpr :: Expr -> Result
transExpr x = case x of
  EOr expr1 expr2 -> failure x
  EAnd expr1 expr2 -> failure x
  ERel expr1 relop expr2 -> failure x
  EAdd expr1 addop expr2 -> failure x
  EMul expr1 mulop expr2 -> failure x
  Not expr -> failure x
  Neg expr -> failure x
  EString string -> failure x
  ELitFalse -> failure x
  ELitTrue -> failure x
  ELitInt integer -> failure x
transAddOp :: AddOp -> Result
transAddOp x = case x of
  Plus -> failure x
  Minus -> failure x
transMulOp :: MulOp -> Result
transMulOp x = case x of
  Times -> failure x
  Div -> failure x
  Mod -> failure x
transRelOp :: RelOp -> Result
transRelOp x = case x of
  LTH -> failure x
  LE -> failure x
  GTH -> failure x
  GE -> failure x
  EQU -> failure x
  NE -> failure x
