module Core.FrontendToCore (
    frontendProgramToCore,
) where

import Data.Foldable (foldrM)
import Data.Functor ((<&>))
import qualified Frontend.AST as F
import Core.Ops
import Core.AST
import Utils.VarSupply

type SuppM = VarSupply String

data ProgArg = ProgVar String (Maybe Type) | ProgTuple [ProgArg] (Maybe Type)

frontendProgramToCore :: F.Program -> [Prog Maybe]
frontendProgramToCore (F.Prog phrases) = 
    phrases >>= (flip evalVarSupply supp . phraseToCore)
    where
        supp = ["&" ++ show x | x <- [(0 :: Int) ..]]

-- it shouldn't be Prog
phraseToCore :: F.Phrase -> SuppM [Prog Maybe]
phraseToCore (F.Value (F.Let binds)) = mapM bindToProg binds
phraseToCore _ = undefined

bindToProg :: F.LetBind -> SuppM (Prog Maybe)
bindToProg (F.ProcBind name vis rType e) = do
    (vis'', e') <- extractLam e
    vis' <- mapM extractName vis
    e'' <- exprToCore e'
    (args, e''') <- extractProgArgs (vis' ++ vis'') e''
    let lTypes = map getProgArgType vis'
    let fType = case (sequence lTypes, rType) of
            (Just tps, Just r) -> 
                Just $ foldr TFun (typeToCore r) tps
            _ -> Nothing
    return $ Prog (VarId name fType) args e'''
bindToProg F.ConstBind {} = undefined

exprToCore :: F.Expr -> SuppM (Expr Maybe)
exprToCore (F.EId var) = return $ Var (VarId var Nothing)
exprToCore (F.EFloat f) = return . Lit $ LFloat f
exprToCore  F.ETrue = return . Lit $ LBool True
exprToCore  F.EFalse = return . Lit $ LBool False
exprToCore (F.EApp e1 e2) = App <$> exprToCore e1 <*> exprToCore e2
exprToCore (F.ETyped (F.EId var) t) = 
    return . Var . VarId var . Just $ typeToCore t
exprToCore (F.ETyped _e _t) = undefined
exprToCore (F.ENeg e) = unOpToCore OpNeg e
exprToCore (F.ENot e) = unOpToCore OpNot e
exprToCore (F.EMul e1 e2) = binOpToCore OpMul e1 e2
exprToCore (F.EDiv e1 e2) = binOpToCore OpDiv e1 e2
exprToCore (F.EMod e1 e2) = binOpToCore OpMod e1 e2
exprToCore (F.EAdd e1 e2) = binOpToCore OpAdd e1 e2
exprToCore (F.ESub e1 e2) = binOpToCore OpSub e1 e2
exprToCore (F.ELTH e1 e2) = binOpToCore OpLT e1 e2
exprToCore (F.ELE e1 e2) = do
    te1 <- exprToCore e1
    te2 <- exprToCore e2
    return $ BinOp OpOr (BinOp OpLT te1 te2) (BinOp OpEq te1 te2)
exprToCore (F.EGTH e1 e2) = UnOp OpNot <$> exprToCore (F.ELE e1 e2)
exprToCore (F.EGE e1 e2) = UnOp OpNot <$> exprToCore (F.ELTH e1 e2)
exprToCore (F.EEQU e1 e2) = binOpToCore OpEq e1 e2
exprToCore (F.ENE e1 e2) = UnOp OpNeg <$> binOpToCore OpEq e1 e2
exprToCore (F.EAnd e1 e2) = binOpToCore OpAnd e1 e2
exprToCore (F.EOr e1 e2) = binOpToCore OpOr e1 e2
exprToCore (F.ECond cond e1 e2) = 
    If <$> exprToCore cond <*> exprToCore e1 <*> exprToCore e2
exprToCore (F.ELetIn (F.Let binds) e) = 
    exprToCore e >>= \te -> foldrM letBindToCore te binds
exprToCore (F.ELambda _lambdas _e) = undefined
exprToCore (F.ETuple exprs) = do
    (funs, tuple) <- foldrM foldFn ([], []) exprs
    return $ foldr LetFun (TupleCons tuple) funs
    where
        foldFn (F.ELambda lambdas e) (fns, exs) = do
            (args, e') <- extractLam e
            args' <- (++ args) <$> mapM extractName lambdas
            e'' <- exprToCore e'
            tmp <- flip VarId Nothing <$> nextVar
            (args'', e''') <- extractProgArgs args' e''
            return (Prog tmp args'' e''':fns, Var tmp:exs)
        foldFn e (fns, exs) = exprToCore e <&> \x -> (fns, x:exs)
exprToCore (F.EInt _i) = undefined
exprToCore (F.EFieldGet _expr _field) = undefined
exprToCore (F.ECons _fields) = undefined
exprToCore (F.ENamedCons _name _fields) = undefined

letBindToCore :: F.LetBind -> Expr Maybe -> SuppM (Expr Maybe)
letBindToCore (F.ConstBind (F.LambdaVId n) e1) e2 = do
    (args, e1') <- extractLam e1
    e1'' <- exprToCore e1'
    if null args 
        then return $ Let (VarId n Nothing) e1'' e2 
        else do 
            (args', e1''') <- extractProgArgs args e1''
            return $ LetFun (Prog (VarId n Nothing) args' e1''') e2
letBindToCore (F.ConstBind (F.TypedVId _lvi _t) _e1) _e2 = undefined
letBindToCore (F.ConstBind F.WildVId _) e2 = return e2
letBindToCore (F.ConstBind (F.TupleVId lambdas) e1) e2 = do
    (args, e1') <- extractLam e1
    lambdas' <- mapM extractName lambdas
    e1'' <- exprToCore e1'
    if not (null args) then undefined else do
        tmp <- flip VarId Nothing <$> nextVar
        return $ Let tmp e1'' $ 
            foldr (\(i, el) -> f el (TupleProj i (Var tmp))) e2 (zip [0..] lambdas')
    where
        f :: ProgArg -> Expr Maybe -> Expr Maybe -> Expr Maybe
        f (ProgVar n ty) e1' e2' = Let (VarId n ty) e1' e2'
        f (ProgTuple tuple _ty) e1' e2' = 
            foldr (\(i, el) -> f el (TupleProj i e1')) e2' (zip [0..] tuple)
letBindToCore (F.ProcBind pName lambdas rType e1) e2 = do
    (args, e1') <- extractLam e1
    lambdas' <- mapM extractName lambdas
    let args' = lambdas' ++ args
    e1'' <- exprToCore e1'
    (args'', e1''') <- extractProgArgs args' e1''
    let lTypes = map getProgArgType lambdas'
        fType = case (sequence lTypes, rType) of
            (Just tps, Just r) -> Just $ foldr TFun (typeToCore r) tps
            _ -> Nothing
    return $ LetFun (Prog (VarId pName fType) args'' e1''') e2

extractProgArgs :: [ProgArg] -> Expr Maybe -> SuppM ([VarId Maybe], Expr Maybe)
extractProgArgs (ProgVar n ty:t) e = do
    (args, e') <- extractProgArgs t e
    return (VarId n ty:args, e')
extractProgArgs (ProgTuple tuple ty:t) e = do
    (args, e') <- extractProgArgs t e
    tmp <- flip VarId ty <$> nextVar
    proj <- foldrM (\(i, el) acc -> do
        (single, e'') <- extractProgArgs [el] acc
        let [tmp'] = single
        return $ Let tmp' (TupleProj i (Var tmp)) e''
        ) e' (zip [0..] tuple)
    return (tmp:args, proj)
extractProgArgs [] e = return ([], e)

getProgArgType :: ProgArg -> Maybe Type
getProgArgType (ProgVar _ t) = t
getProgArgType (ProgTuple _ t) = t

extractLam :: F.Expr -> SuppM ([ProgArg], F.Expr)
extractLam (F.ELambda vis e) = do
    (args, e') <- extractLam e
    vis' <- mapM extractName vis
    return (vis' ++ args, e')
extractLam e = return ([], e)

-- FOR NOW
extractName :: F.LambdaVI -> SuppM ProgArg
extractName (F.LambdaVId n) = return (ProgVar n Nothing)
extractName F.WildVId = (`ProgVar` Nothing) <$> nextVar
extractName (F.TypedVId n t) = do
    n' <- extractName n
    let t' = typeToCore t
    case n' of
        ProgVar nvar _ -> return (ProgVar nvar $ Just t')
        ProgTuple tpl _ -> return (ProgTuple tpl $ Just t')
extractName (F.TupleVId lambdas) = do
    lambdas' <- mapM extractName lambdas
    return $ ProgTuple lambdas' Nothing

unOpToCore :: UnOp -> F.Expr -> SuppM (Expr Maybe)
unOpToCore op e = UnOp op <$> exprToCore e

binOpToCore :: BinOp -> F.Expr -> F.Expr -> SuppM (Expr Maybe)
binOpToCore op e1 e2 = BinOp op <$> exprToCore e1 <*> exprToCore e2

typeToCore :: F.Type -> Type
typeToCore F.TInt = undefined
typeToCore F.TFloat = TFloat
typeToCore F.TBool = TBool
typeToCore (F.TStruct structName) = case structName of
    'V':'e':'c':i -> TTuple TFloat (read i)
    _ -> undefined
typeToCore (F.TPoly _) = undefined
typeToCore (F.TFun t1 t2) = TFun (typeToCore t1) (typeToCore t2)
