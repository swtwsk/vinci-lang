module SSA.AST where

import Data.List (intercalate, sort)

import Core.Ops

type VarName = String

data SFnDef = SFnDef String [SArg] SBlock [SLabelledBlock] -- deriving Eq

newtype SBlock = SBlock [SStmt] deriving Eq

data SLabelledBlock = SLabelled SLabel [SPhiNode] SBlock deriving Eq

data SPhiNode = SPhiNode VarName [(SLabel, SArg)] deriving Eq

data SStmt = SAssign VarName SExpr
           | SGoto SLabel
           | SReturn SExpr
           | SIf SExpr SLabel SLabel
           deriving Eq

data SExpr = SVar VarName
           | SApp VarName [VarName]
           | STupleCtr [VarName]
           | STupleProj Int VarName
           | SBinOp BinOp SExpr SExpr
           | SUnOp UnOp SExpr
           | SLitFloat Double
           | SLitBool Bool
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
    show (SIf cond b1 b2) = "if (" ++ show cond ++ ") { goto " ++ show b1 ++ 
        "} else { goto " ++ show b2 ++ "}"

instance Show SLabel where
    show (SLabel l) = l

instance Show SArg where
    show (SArg arg) = arg

instance Show SExpr where
    show (SVar v) = v
    show (SApp f args) = f ++ "(" ++ intercalate "," args ++ ")"
    show (STupleCtr vars) = "(" ++ intercalate ", " vars ++ ")"
    show (STupleProj i v) = "π" ++ show i ++ "(" ++ v ++ ")"
    show (SBinOp op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
    show (SUnOp op e) = show op ++ " " ++ show e
    show (SLitFloat f) = show f
    show (SLitBool b) = show b
