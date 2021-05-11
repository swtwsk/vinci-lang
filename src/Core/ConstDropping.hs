{-# LANGUAGE LambdaCase #-}
module Core.ConstDropping (dropConsts) where

import Control.Monad.Reader
import qualified Data.Map as Map

import Core.AST
import Core.CoreManager
import Core.ExpandTypeSynonyms (expandTypeSynonyms)

type DropM = Reader (Map.Map (VarId Maybe) (Binding Maybe))

dropConsts :: CoreBindingsManager Maybe -> CoreManager Maybe
dropConsts cbm = CoreManager { _progs = progs'
                             , _structDefs = _bindStructDefs cbm }
    where
        progs  = dropConsts' (_bindings cbm)
        progs' = expandTypeSynonyms (_bindTypeSynonyms cbm) <$> progs

dropConsts' :: [Binding Maybe] -> [Prog Maybe]
dropConsts' bindings = bindings >>= \case
        ProgBinding prog -> [runReader (dropConstsProg prog) bindingsMap]
        ConstBinding _ _ -> []
    where
        bindingsMap = foldr (\el -> Map.insert (bindingName el) el) Map.empty bindings

        bindingName :: Binding Maybe -> VarId Maybe
        bindingName (ProgBinding (Prog v _ _)) = v
        bindingName (ConstBinding v _) = v

dropConstsProg :: Prog Maybe -> DropM (Prog Maybe)
dropConstsProg (Prog progName args expr) = 
    Prog progName args <$> dropConstsExpr expr

dropConstsExpr :: Expr Maybe -> DropM (Expr Maybe)
dropConstsExpr (Var x) = do
    bindingMap <- ask
    case Map.lookup x bindingMap of
        Just (ConstBinding _ e) -> return e
        _ -> return (Var x)
dropConstsExpr (Lit l) = return (Lit l)
dropConstsExpr (App e1 e2) = App <$> dropConstsExpr e1 <*> dropConstsExpr e2
dropConstsExpr (If cond e1 e2) = 
    If <$> dropConstsExpr cond <*> dropConstsExpr e1 <*> dropConstsExpr e2
dropConstsExpr (Cons sName exprs) = Cons sName <$> mapM dropConstsExpr exprs
dropConstsExpr (FieldGet sName e) = FieldGet sName <$> dropConstsExpr e
dropConstsExpr (TupleCons exprs) = TupleCons <$> mapM dropConstsExpr exprs
dropConstsExpr (TupleProj i e) = TupleProj i <$> dropConstsExpr e
dropConstsExpr (Let f e1 e2) = do
    e1' <- dropConstsExpr e1
    e2' <- local (Map.delete f) (dropConstsExpr e2)
    return $ Let f e1' e2'
dropConstsExpr (LetFun p@(Prog f _ _) e2) = do
    p'  <- local (Map.delete f) (dropConstsProg p)
    e2' <- local (Map.delete f) (dropConstsExpr e2)
    return $ LetFun p' e2'
dropConstsExpr (BinOp op e1 e2) = 
    BinOp op <$> dropConstsExpr e1 <*> dropConstsExpr e2
dropConstsExpr (UnOp op e) = UnOp op <$> dropConstsExpr e
