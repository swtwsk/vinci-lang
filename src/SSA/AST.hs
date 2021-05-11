module SSA.AST where

import Data.Bifunctor (first)
import Data.List (intercalate, sort)

import Core.Ops
import SPIRV.Types

type VarName = String
data Var = Var { _varName :: VarName, _varType :: SpirType } deriving Eq

data SFnDef = SFnDef String SpirType [SArg] SBlock [SLabelledBlock] -- deriving Eq

newtype SBlock = SBlock [SStmt] deriving Eq

data SLabelledBlock = SLabelled SLabel [SPhiNode] SBlock deriving Eq

data SPhiNode = SPhiNode Var [(SLabel, String)] deriving Eq

data SStmt = SAssign Var SExpr
           | SGoto SLabel
           | SReturn SExpr
           | SIf (Maybe SStructuredMerge) SExpr SLabel SLabel
           deriving Eq

data SExpr = SVar Var
           | SApp Var [Var]
           | SStructCtr SpirType [Var]  -- also TupleCtr
           | SStructGet Int Var
           | STupleProj Int Var
           | SBinOp BinOp SExpr SExpr
           | SUnOp UnOp SExpr
           | SLitFloat Double
           | SLitBool Bool
           | SLitInt Int
           deriving Eq

data SStructuredMerge = SLoopMerge SLabel SLabel 
                      | SSelectionMerge SLabel
                      deriving Eq

newtype SArg = SArg Var deriving Eq
newtype SLabel = SLabel String deriving (Eq, Ord)

-- EQ
instance Ord SLabelledBlock where
    (SLabelled l1 _ _) <= (SLabelled l2 _ _) = l1 <= l2

instance Eq SFnDef where
    (SFnDef f1 rt1 args1 b1 lbs1) == (SFnDef f2 rt2 args2 b2 lbs2) =
        f1 == f2 && rt1 == rt2 && args1 == args2 && b1 == b2 && 
        sort lbs1 == sort lbs2

-- SHOWS
instance Show Var where
    show (Var varName varType) = 
        "(" ++ varName ++ " : " ++ show varType ++ ")"

instance Show SFnDef where
    show (SFnDef fName rType args block labelledBlocks) = fName ++ "(" ++ 
        intercalate "," (show <$> args) ++ ") : " ++ show rType ++ 
        " {\n" ++ show block ++ "\n" ++
        unlines (show <$> labelledBlocks) ++ "}\n"

instance Show SBlock where
    show (SBlock stmts) = intercalate "\n" $ (\x -> "    " ++ show x) <$> stmts

instance Show SLabelledBlock where
    show (SLabelled l phiNodes block) = "  " ++ show l ++ ":\n" ++ 
        unlines ((\x -> "    " ++ show x) <$> phiNodes) ++ show block

instance Show SPhiNode where
    show (SPhiNode v exprs) = 
        show v ++ " <- Φ(" ++ intercalate "," (showTuple . first show <$> exprs) ++ ")"
        where
            showTuple (a, b) = "(" ++ a ++ ", " ++ b ++ ")"

instance Show SStmt where
    show (SAssign v e) = show v ++ " <- " ++ show e ++ ";"
    show (SGoto l) = "goto " ++ show l ++ ";"
    show (SReturn e) = "return " ++ show e ++ ";"
    show (SIf scf cond b1 b2) = show scf ++ " -> if (" ++ show cond ++ ") { goto " ++ show b1 ++ 
        " } else { goto " ++ show b2 ++ " }"

instance Show SStructuredMerge where
    show (SLoopMerge l1 l2) = "merge " ++ show l1 ++ " " ++ show l2
    show (SSelectionMerge l) = "select " ++ show l

instance Show SLabel where
    show (SLabel l) = l

instance Show SArg where
    show (SArg arg) = show arg

instance Show SExpr where
    show (SVar v) = show v
    show (SApp f args) = show f ++ "(" ++ intercalate "," (show <$> args) ++ ")"
    show (SStructCtr sType vars) = 
        show sType ++ " { " ++ intercalate ", " (show <$> vars) ++ " }"
    show (SStructGet i v) = "get" ++ show i ++ "(" ++ show v ++ ")"
    show (STupleProj i v) = "π" ++ show i ++ "(" ++ show v ++ ")"
    show (SBinOp op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
    show (SUnOp op e) = show op ++ " " ++ show e
    show (SLitFloat f) = show f
    show (SLitBool b)  = show b
    show (SLitInt i)   = show i
