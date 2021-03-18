{-# LANGUAGE MultiParamTypeClasses #-}

module Frontend.TranspileAST (
    transpile,
) where

import Frontend.AST
import qualified AST.AbsVinci as Abs

class Transpilable a b where
    transpile :: a -> b

instance Transpilable (Abs.Program a) Program where
    transpile (Abs.Prog _ phrase) = Prog (transpile <$> phrase)

instance Transpilable (Abs.Line a) Line where
    transpile (Abs.Line _ phrase) = Line (transpile phrase)

instance Transpilable (Abs.Phrase a) Phrase where
    transpile (Abs.Value _ letdef) = Value (transpile letdef)
    transpile (Abs.Expression _ expr) = Expression (transpile expr)
    transpile (Abs.StructDecl _ sdef) = StructDecl (transpile sdef)

instance Transpilable (Abs.LetDef a) LetDef where
    transpile (Abs.Let _ letbind) = Let (transpile <$> letbind)
    transpile (Abs.LetRec _ letbind) = LetRec (transpile <$> letbind)

instance Transpilable (Abs.LetBind a) LetBind where
    transpile letbind = case letbind of
        Abs.ConstBind _ letlvi expr -> ConstBind (transpileLetLVI letlvi) (transpile expr)
        Abs.ProcBind _ (Abs.ProcNameId _ procName) letlvis rtype expr -> ProcBind (extractVIdent procName) (transpileLetLVI <$> letlvis) (transpileRType rtype) (transpile expr)
        where
            transpileLetLVI :: Abs.LetLVI a -> LambdaVI
            transpileLetLVI (Abs.LetLVI _ lambdavi) = transpile lambdavi
            transpileRType :: Abs.RType a -> Maybe Type
            transpileRType rtype = case rtype of
                Abs.NoRetType _ -> Nothing
                Abs.RetType _ t -> Just $ transpile t

instance Transpilable (Abs.Expr a) Expr where
    transpile (Abs.EId _ vident) = EId $ extractVIdent vident
    transpile (Abs.EInt _ i) = EInt i
    transpile (Abs.EFloat _ f) = EFloat f
    transpile (Abs.ETrue _) = ETrue
    transpile (Abs.EFalse _) = EFalse
    transpile (Abs.EFieldGet _ expr vident) = EFieldGet (transpile expr) (extractVIdent vident)
    transpile (Abs.EApp _ e1 e2) = EApp (transpile e1) (transpile e2)
    transpile (Abs.ETyped _ expr t) = ETyped (transpile expr) (transpile t)
    transpile (Abs.ENeg _ expr) = ENeg $ transpile expr
    transpile (Abs.ENot _ expr) = ENot $ transpile expr
    transpile (Abs.EMul _ e1 e2) = EMul (transpile e1) (transpile e2) 
    transpile (Abs.EDiv _ e1 e2) = EDiv (transpile e1) (transpile e2)
    transpile (Abs.EMod _ e1 e2) = EMod (transpile e1) (transpile e2)
    transpile (Abs.EAdd _ e1 e2) = EAdd (transpile e1) (transpile e2)
    transpile (Abs.ESub _ e1 e2) = ESub (transpile e1) (transpile e2)
    transpile (Abs.ELTH _ e1 e2) = ELTH (transpile e1) (transpile e2)
    transpile (Abs.ELE _ e1 e2) = ELE (transpile e1) (transpile e2)
    transpile (Abs.EGTH _ e1 e2) = EGTH (transpile e1) (transpile e2)
    transpile (Abs.EGE _ e1 e2) = EGE (transpile e1) (transpile e2)
    transpile (Abs.EEQU _ e1 e2) = EEQU (transpile e1) (transpile e2)
    transpile (Abs.ENE _ e1 e2) = ENE (transpile e1) (transpile e2)
    transpile (Abs.EAnd _ e1 e2) = EAnd (transpile e1) (transpile e2)
    transpile (Abs.EOr _ e1 e2) = EOr (transpile e1) (transpile e2)
    transpile (Abs.ECond _ cond e1 e2) = ECond (transpile cond) (transpile e1) (transpile e2)
    transpile (Abs.ELetIn _ letdef expr) = ELetIn (transpile letdef) (transpile expr)
    transpile (Abs.ELambda _ lambdavis expr) = ELambda (transpile <$> lambdavis) (transpile expr)
    transpile (Abs.ETuple _ e1 exprs) = ETuple $ transpile e1 : (transpile <$> exprs)
    transpile (Abs.ENamedCons _ sident fielddefs) = ENamedCons (extractSIdent sident) (transpile <$> fielddefs)
    transpile (Abs.ECons _ fielddefs) = ECons (transpile <$> fielddefs)

instance Transpilable (Abs.LambdaVI a) LambdaVI where
    transpile (Abs.LambdaVId _ vident) = LambdaVId (extractVIdent vident)
    transpile (Abs.TypedVId _ lambdavi t) = TypedVId (transpile lambdavi) (transpile t)
    transpile (Abs.WildVId _) = WildVId
    transpile (Abs.TupleVId _ lambdavis) = TupleVId $ transpile <$> lambdavis

instance Transpilable (Abs.FieldDef a) FieldDef where
    transpile (Abs.FieldDef _ vident expr) = FieldDef (extractVIdent vident) (transpile expr)

instance Transpilable (Abs.StructDef a) StructDef where
    transpile (Abs.SDef _ sident polyidents fielddecls) = SDef (extractSIdent sident) (extractPolyIdent <$> polyidents) (transpile <$> fielddecls)

instance Transpilable (Abs.FieldDecl a) FieldDecl where
    transpile (Abs.FieldDecl _ vident t) = FieldDecl (extractVIdent vident) (transpile t)

instance Transpilable (Abs.Type a) Type where
    transpile (Abs.TInt _) = TInt
    transpile (Abs.TFloat _) = TFloat
    transpile (Abs.TBool _) = TBool
    transpile (Abs.TStruct _ sident) = TStruct (extractSIdent sident)
    transpile (Abs.TPoly _ polyIdent) = TPoly (extractPolyIdent polyIdent)
    transpile (Abs.TFun _ t1 t2) = TFun (transpile t1) (transpile t2)

extractVIdent :: Abs.VIdent -> String
extractVIdent (Abs.VIdent ident) = ident

extractSIdent :: Abs.SIdent -> String
extractSIdent (Abs.SIdent ident) = ident

extractPolyIdent :: Abs.TPolyIdent -> String
extractPolyIdent (Abs.TPolyIdent ident) = ident
