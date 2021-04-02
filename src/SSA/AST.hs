module SSA.AST where

import Data.List (intercalate, sort)

type VarName = String

data SFnDef = SFnDef String [SArg] SBlock [SLabelledBlock] -- deriving Eq

newtype SBlock = SBlock [SStmt] deriving Eq

data SLabelledBlock = SLabelled SLabel [SPhiNode] SBlock deriving Eq

data SPhiNode = SPhiNode VarName [(SLabel, SArg)] deriving Eq

data SStmt = SAssign VarName SExpr
           | SGoto SLabel
           | SReturn SExpr
           | SIf SExpr SBlock SBlock
           deriving Eq

data SExpr = SVar VarName
           | SApp VarName [VarName]
           | SAdd SExpr SExpr
           | SMul SExpr SExpr
           | SLT SExpr SExpr
           | SLitFloat Double
           deriving Eq

newtype SArg = SArg VarName deriving Eq
newtype SLabel = SLabel String deriving (Eq, Ord)

-- EQ
instance Ord SLabelledBlock where
    (SLabelled l1 _ _) <= (SLabelled l2 _ _) = l1 <= l2

instance Eq SFnDef where
    (SFnDef f1 args1 b1 lbs1) == (SFnDef f2 args2 b2 lbs2) =
        f1 == f2 && args1 == args2 && b1 == b2 && sort lbs1 == sort lbs2

-- SHOWS
instance Show SFnDef where
    show (SFnDef fName args block labelledBlocks) = fName ++ "(" ++ 
        intercalate "," (show <$> args) ++ ") {\n" ++ show block ++ "\n" ++
        unlines (show <$> labelledBlocks) ++ "}\n"

instance Show SBlock where
    show (SBlock stmts) = intercalate "\n" $ (\x -> "    " ++ show x) <$> stmts

instance Show SLabelledBlock where
    show (SLabelled l phiNodes block) = " " ++ show l ++ ": " ++ 
        unlines (show <$> phiNodes) ++ show block

instance Show SPhiNode where
    show (SPhiNode v exprs) = v ++ " <- Φ(" ++ intercalate "," (show <$> exprs) ++ ")"

instance Show SStmt where
    show (SAssign v e) = v ++ " <- " ++ show e ++ ";"
    show (SGoto l) = "goto " ++ show l ++ ";"
    show (SReturn e) = "return " ++ show e ++ ";"
    show (SIf cond b1 b2) = "if (" ++ show cond ++ ") {" ++ show b1 ++ 
        "} else {" ++ show b2 ++ "}"

instance Show SLabel where
    show (SLabel l) = l

instance Show SArg where
    show (SArg arg) = arg

instance Show SExpr where
    show (SVar v) = v
    show (SApp f args) = f ++ "(" ++ intercalate "," args ++ ")"
    show (SAdd e1 e2) = show e1 ++ " + " ++ show e2
    show (SMul e1 e2) = show e1 ++ " * " ++ show e2
    show (SLT e1 e2)  = show e1 ++ " < " ++ show e2
    show (SLitFloat f) = show f
