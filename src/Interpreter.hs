module Interpreter where

  import AbsStarsepLang
  import ErrM
  type Result = Err String

  failure :: Show a => a -> Result
  failure x = Bad $ "Undefined case: " ++ show x

  transIdent :: Ident -> Result
  transIdent x = case x of
    Ident string -> failure x
  transProgram :: Program -> Result
  transProgram x = case x of
    ProgramDecl topdefs -> failure x
  transTopDef :: TopDef -> Result
  transTopDef x = case x of
    FnDef type_ ident args block -> failure x
  transArg :: Arg -> Result
  transArg x = case x of
    ArgDecl type_ ident -> failure x
  transBlock :: Block -> Result
  transBlock x = case x of
    BlockDecl stmts -> failure x
  transStmt :: Stmt -> Result
  transStmt x = case x of
    BStmt block -> failure x
    Decl type_ items -> failure x
    Let ident expr -> failure x
    Ass ident expr -> failure x
    Incr ident -> failure x
    Decr ident -> failure x
    Ret expr -> failure x
    VRet -> failure x
    While expr block -> failure x
    For stmt1 expr stmt2 block -> failure x
    Loop block -> failure x
    FunExec ident exprs -> failure x
    CondIf ifstmt -> failure x
    ElseStmt ifelsestmt -> failure x
  transItem :: Item -> Result
  transItem x = case x of
    NoInit ident -> failure x
    Init ident expr -> failure x
  transIfStmt :: IfStmt -> Result
  transIfStmt x = case x of
    CondElif ifstmt expr block -> failure x
    Cond expr block -> failure x
  transIfElseStmt :: IfElseStmt -> Result
  transIfElseStmt x = case x of
    CondElse ifstmt block -> failure x
  transType :: Type -> Result
  transType x = case x of
    Int -> failure x
    Str -> failure x
    Bool -> failure x
    Void -> failure x
    TypeOf expr -> failure x
    Fun type_ types -> failure x
  transExpr :: Expr -> Result
  transExpr x = case x of
    EInt integer -> failure x
    EString string -> failure x
    EFalse -> failure x
    ETrue -> failure x
    Neg expr -> failure x
    Not expr -> failure x
    EMul expr1 mulop expr2 -> failure x
    EAdd expr1 addop expr2 -> failure x
    ERel expr1 relop expr2 -> failure x
    EAnd expr1 expr2 -> failure x
    EOr expr1 expr2 -> failure x
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
