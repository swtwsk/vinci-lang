module AST.SkelVinci where

-- Haskell module generated by the BNF converter

import AST.AbsVinci
import AST.ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transVIdent :: VIdent -> Result
transVIdent x = case x of
  VIdent string -> failure x
transSIdent :: SIdent -> Result
transSIdent x = case x of
  SIdent string -> failure x
transTPolyIdent :: TPolyIdent -> Result
transTPolyIdent x = case x of
  TPolyIdent string -> failure x
transProgram :: Show a => Program a -> Result
transProgram x = case x of
  Prog _ phrases -> failure x
transLine :: Show a => Line a -> Result
transLine x = case x of
  Line _ phrase -> failure x
transPhrase :: Show a => Phrase a -> Result
transPhrase x = case x of
  Value _ letdef -> failure x
  Expression _ expr -> failure x
  StructDecl _ structdef -> failure x
transLetDef :: Show a => LetDef a -> Result
transLetDef x = case x of
  Let _ letbinds -> failure x
  LetRec _ letbinds -> failure x
transLetBind :: Show a => LetBind a -> Result
transLetBind x = case x of
  ConstBind _ letlvi expr -> failure x
  ProcBind _ procname letlvis rtype expr -> failure x
transLetLVI :: Show a => LetLVI a -> Result
transLetLVI x = case x of
  LetLVI _ lambdavi -> failure x
transExpr :: Show a => Expr a -> Result
transExpr x = case x of
  EId _ vident -> failure x
  EInt _ integer -> failure x
  EFloat _ double -> failure x
  ETrue _ -> failure x
  EFalse _ -> failure x
  EFieldGet _ expr vident -> failure x
  ETuple _ expr exprs -> failure x
  EApp _ expr1 expr2 -> failure x
  ETyped _ expr type_ -> failure x
  ENeg _ expr -> failure x
  ENot _ expr -> failure x
  EMul _ expr1 expr2 -> failure x
  EDiv _ expr1 expr2 -> failure x
  EMod _ expr1 expr2 -> failure x
  EAdd _ expr1 expr2 -> failure x
  ESub _ expr1 expr2 -> failure x
  ELTH _ expr1 expr2 -> failure x
  ELE _ expr1 expr2 -> failure x
  EGTH _ expr1 expr2 -> failure x
  EGE _ expr1 expr2 -> failure x
  EEQU _ expr1 expr2 -> failure x
  ENE _ expr1 expr2 -> failure x
  EAnd _ expr1 expr2 -> failure x
  EOr _ expr1 expr2 -> failure x
  ECond _ expr1 expr2 expr3 -> failure x
  ELetIn _ letdef expr -> failure x
  ELambda _ lambdavis expr -> failure x
  ENamedCons _ sident fielddefs -> failure x
  ECons _ fielddefs -> failure x
transLambdaVI :: Show a => LambdaVI a -> Result
transLambdaVI x = case x of
  TypedVId _ lambdavi type_ -> failure x
  LambdaVId _ vident -> failure x
  WildVId _ -> failure x
  TupleVId _ lambdavis -> failure x
transFieldDef :: Show a => FieldDef a -> Result
transFieldDef x = case x of
  FieldDef _ vident expr -> failure x
transProcName :: Show a => ProcName a -> Result
transProcName x = case x of
  ProcNameId _ vident -> failure x
transStructDef :: Show a => StructDef a -> Result
transStructDef x = case x of
  SDef _ sident tpolyidents fielddecls -> failure x
transFieldDecl :: Show a => FieldDecl a -> Result
transFieldDecl x = case x of
  FieldDecl _ vident type_ -> failure x
transType :: Show a => Type a -> Result
transType x = case x of
  TInt _ -> failure x
  TFloat _ -> failure x
  TBool _ -> failure x
  TStruct _ sident -> failure x
  TPoly _ tpolyident -> failure x
  TFun _ type_1 type_2 -> failure x
transRType :: Show a => RType a -> Result
transRType x = case x of
  NoRetType _ -> failure x
  RetType _ type_ -> failure x
