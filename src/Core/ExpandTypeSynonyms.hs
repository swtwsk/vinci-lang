module Core.ExpandTypeSynonyms (expandTypeSynonyms) where

import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

import Core.AST
import Core.CoreManager
import Core.Types (Type(..))

type ExpandM = Reader TypeSynonymsMap

expandTypeSynonyms :: TypeSynonymsMap -> Prog Maybe -> Prog Maybe
expandTypeSynonyms synonyms prog = runReader (expandProg prog) synonyms

expandProg :: Prog Maybe -> ExpandM (Prog Maybe)
expandProg (Prog v args e) = do
    v' <- expandVar v
    args' <- mapM expandVar args
    e' <- expandExpr e
    return $ Prog v' args' e'

expandExpr :: Expr Maybe -> ExpandM (Expr Maybe)
expandExpr (Var v) = Var <$> expandVar v
expandExpr (Lit l) = return (Lit l)
expandExpr (App e1 e2) = App <$> expandExpr e1 <*> expandExpr e2
expandExpr (If c e1 e2) = 
    If <$> expandExpr c <*> expandExpr e1 <*> expandExpr e2
expandExpr (Cons sName exprs) = Cons sName <$> mapM expandExpr exprs
expandExpr (FieldGet sName e) = FieldGet sName <$> expandExpr e
expandExpr (TupleCons exprs) = TupleCons <$> mapM expandExpr exprs
expandExpr (TupleProj i e) = TupleProj i <$> expandExpr e
expandExpr (Let fv e1 e2) =
    Let <$> expandVar fv <*> expandExpr e1 <*> expandExpr e2
expandExpr (LetFun prog e) = LetFun <$> expandProg prog <*> expandExpr e
expandExpr (BinOp op e1 e2) = BinOp op <$> expandExpr e1 <*> expandExpr e2
expandExpr (UnOp op e) = UnOp op <$> expandExpr e

expandVar :: VarId Maybe -> ExpandM (VarId Maybe)
expandVar (VarId v (Just t)) = do
    tSynonym <- expandType t
    return $ VarId v (Just tSynonym)
expandVar v = return v

expandType :: Type -> ExpandM Type
expandType (TFun t1 t2) = TFun <$> expandType t1 <*> expandType t2
expandType (TTuple t i) = flip TTuple i <$> expandType t
expandType t@(TStruct _) = do
    tSynonym <- asks $ Map.lookup t
    return $ fromMaybe t tSynonym
expandType t = return t
