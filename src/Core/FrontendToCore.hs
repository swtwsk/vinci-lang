module Core.FrontendToCore where

import qualified Frontend.AST as F
import Core.Ops
import Core.AST

-- import Control.Monad
-- import Control.Monad.Writer
-- import Data.DList ( DList, toList )
-- import Utils.DList (output)

-- type CoreM = Writer (DList Expr)

frontendProgramToCore :: F.Program -> [Prog]
frontendProgramToCore (F.Prog _phrases) = undefined

exprToCore :: F.Expr -> Expr
exprToCore (F.EId var) = Var var
exprToCore (F.EFloat f) = Lit $ LFloat f
exprToCore  F.ETrue = Lit $ LBool True
exprToCore  F.EFalse = Lit $ LBool False
exprToCore (F.EApp e1 e2) = App (exprToCore e1) (exprToCore e2)
exprToCore (F.ENeg e) = unOpToCore OpNeg e
exprToCore (F.ENot e) = unOpToCore OpNot e
exprToCore (F.EMul e1 e2) = binOpToCore OpMul e1 e2
exprToCore (F.EDiv e1 e2) = binOpToCore OpDiv e1 e2
exprToCore (F.EAdd e1 e2) = binOpToCore OpAdd e1 e2
exprToCore (F.ESub e1 e2) = binOpToCore OpSub e1 e2
exprToCore (F.ELTH e1 e2) = binOpToCore OpLT e1 e2
exprToCore (F.ELE e1 e2) =
    BinOp OpOr (BinOp OpLT te1 te2) (BinOp OpEq te1 te2)
    where
        te1 = exprToCore e1
        te2 = exprToCore e2
exprToCore (F.EGTH e1 e2) = UnOp OpNot $ exprToCore (F.ELE e1 e2)
exprToCore (F.EGE e1 e2) = UnOp OpNot $ exprToCore (F.ELTH e1 e2)
exprToCore (F.EEQU e1 e2) = binOpToCore OpEq e1 e2
exprToCore (F.ENE e1 e2) = UnOp OpNeg $ binOpToCore OpEq e1 e2
exprToCore (F.EAnd e1 e2) = binOpToCore OpAnd e1 e2
exprToCore (F.EOr e1 e2) = binOpToCore OpOr e1 e2
exprToCore (F.ECond cond e1 e2) = 
    If (exprToCore cond) (exprToCore e1) (exprToCore e2)
exprToCore (F.ELetIn letdef e) = case letdef of
    F.Let binds -> foldr letBindToCore te binds
    F.LetRec binds -> foldr letRecBindToCore te binds
    where
        te = exprToCore e
exprToCore (F.ELambda lambdas e) = 
    foldr Lam (exprToCore e) $ extractName <$> lambdas
exprToCore (F.ETuple _exprs) = undefined
exprToCore (F.EInt _i) = undefined
exprToCore (F.EFieldGet _expr _field) = undefined
exprToCore (F.ETyped _expr _type) = undefined
exprToCore (F.EMod _e1 _e2) = undefined
exprToCore (F.ECons _fields) = undefined
exprToCore (F.ENamedCons _name _fields) = undefined

letBindToCore :: F.LetBind -> Expr -> Expr
letBindToCore (F.ConstBind (F.LambdaVId n) e1) e2 = Let n (exprToCore e1) e2
letBindToCore F.ConstBind {} _ = undefined
letBindToCore (F.ProcBind pName lambdas _type e1) e2 = Let pName lambdas' e2
    where
        lambdas' = foldr Lam (exprToCore e1) $ extractName <$> lambdas

letRecBindToCore :: F.LetBind -> Expr -> Expr
letRecBindToCore (F.ProcBind pName lambdas _type e1) e2 =
    LetRec pName (extractName <$> lambdas) (exprToCore e1) e2
letRecBindToCore F.ConstBind {} _ = undefined

-- FOR NOW
extractName :: F.LambdaVI -> String
extractName (F.LambdaVId n) = n
extractName _ = undefined

unOpToCore :: UnOp -> F.Expr -> Expr
unOpToCore op e = UnOp op $ exprToCore e

binOpToCore :: BinOp -> F.Expr -> F.Expr -> Expr
binOpToCore op e1 e2 = BinOp op (exprToCore e1) (exprToCore e2)
