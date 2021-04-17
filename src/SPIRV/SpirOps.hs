module SPIRV.SpirOps where

newtype SpirId = SpirId String  -- for now
               deriving (Eq, Ord)

data SpirOp = OpFunction SpirId SpirId SpirFunctionControl SpirId
            | OpFunctionParameter SpirId SpirId
            | OpLabel SpirId
            | OpBranch SpirId
            | OpBranchConditional SpirId SpirId SpirId
            | OpCompositeConstruct SpirId SpirId [SpirId]
            | OpLoad SpirId SpirId SpirId
            | OpStore SpirId SpirId
            | OpAccessChain SpirId SpirId SpirId SpirId
            | OpVariable SpirId SpirId SpirStorageClass
            | OpPhi SpirId SpirId [(SpirId, SpirId)]
            | OpReturn
            | OpReturnValue SpirId
            | OpFunctionEnd
            | OpFunctionCall SpirId SpirId SpirId [SpirId]
            | OpExtInst SpirId SpirId SpirId String [SpirId]
            | OpConstant SpirId SpirId SpirConst
            | OpFNegate SpirId SpirId SpirId
            | OpFAdd SpirId SpirId SpirId SpirId
            | OpFSub SpirId SpirId SpirId SpirId
            | OpFMul SpirId SpirId SpirId SpirId
            | OpFDiv SpirId SpirId SpirId SpirId
            | OpFMod SpirId SpirId SpirId SpirId
            | OpFOrdEqual SpirId SpirId SpirId SpirId
            | OpFOrdLessThan SpirId SpirId SpirId SpirId
            | OpLogicalOr SpirId SpirId SpirId SpirId
            | OpLogicalAnd SpirId SpirId SpirId SpirId
            | OpLogicalNot SpirId SpirId SpirId

            | OpTypeVoid SpirId
            | OpTypeBool SpirId
            | OpTypeInt SpirId Int Bool
            | OpTypeFloat SpirId Int
            | OpTypeVector SpirId SpirId Int
            | OpTypePointer SpirId SpirStorageClass SpirId
            | OpTypeFunction SpirId SpirId [SpirId]
            deriving Eq

data SpirConst = SCFloat Double
               | SCBool Bool
               | SCUnsigned Int
               deriving Eq

data SpirFunctionControl = FCNone | FCInline | FCDontInline | FCPure | FCConst
                         deriving Eq

data SpirStorageClass = StorFunction deriving (Eq, Ord) -- and other

-- SHOWS
instance Show SpirId where
    show (SpirId sid) = '%':sid

instance Show SpirOp where
    show (OpFunction res resultType functionControl functionType) =
        show res ++ " = OpFunction " ++ show resultType ++ " " ++ 
        show functionControl ++ " " ++ show functionType
    show (OpFunctionParameter res resultType) = 
        showOpWithResult res "OpFunctionParameter" [resultType]
    show (OpLabel res) = showOpWithResult res "OpLabel" []
    show (OpBranch b) = "OpBranch " ++ show b
    show (OpBranchConditional cond b1 b2) = 
        "OpBranchConditional " ++ show cond ++ " " ++ show b1 ++ " " ++ show b2
    show (OpCompositeConstruct res resT args) =
        showOpWithResult res "OpCompositeConstruct" (resT:args)
    show (OpLoad res resultType arg) = 
        showOpWithResult res "OpLoad" [resultType, arg]
    show (OpStore res toStore) = "OpStore " ++ show res ++ " " ++ show toStore
    show (OpAccessChain res resT pointer i) =
        showOpWithResult res "OpAccessChain" [resT, pointer, i]
    show (OpVariable var varType storage) = 
        show var ++ " = OpVariable " ++ show varType ++ " " ++ show storage
    show (OpPhi res resultType varLabels) =
        let pairToList = \(a, b) -> [a, b]
            varLabels' = concat $ pairToList <$> varLabels in
        showOpWithResult res "OpPhi" (resultType:varLabels')
    show OpReturn = "OpReturn"
    show (OpReturnValue arg) = "OpReturnValue " ++ show arg
    show OpFunctionEnd = "OpFunctionEnd"
    show (OpFunctionCall res fType fName args) = 
        showOpWithResult res "OpFunctionCall" (fType:fName:args)
    show (OpExtInst res fType extId fName args) = show res ++ " = OpExtInst " ++ 
        show fType ++ " " ++ show extId ++ " " ++ fName ++ " " ++ 
        (if not (null args) then " " else "") ++ unwords (show <$> args)
    show (OpConstant res resType c) = 
        show res ++ " = OpConstant " ++ show resType ++ " " ++ show c
    show (OpFNegate res resT x) = showOpWithResult res "OpFNegate" [resT, x]
    show (OpFAdd res resT a b) = showOpWithResult res "OpFAdd" [resT, a, b]
    show (OpFSub res resT a b) = showOpWithResult res "OpFSub" [resT, a, b]
    show (OpFMul res resT a b) = showOpWithResult res "OpFMul" [resT, a, b]
    show (OpFDiv res resT a b) = showOpWithResult res "OpFDiv" [resT, a, b]
    show (OpFMod res resT a b) = showOpWithResult res "OpFMod" [resT, a, b]
    show (OpFOrdEqual res resT a b) = 
        showOpWithResult res "OpFOrdEqual" [resT, a, b]
    show (OpFOrdLessThan res resT a b) = 
        showOpWithResult res "OpFOrdLessThan" [resT, a, b]
    show (OpLogicalOr res resT a b) = 
        showOpWithResult res "OpLogicalOr" [resT, a, b]
    show (OpLogicalAnd res resT a b) = 
        showOpWithResult res "OpLogicalAnd" [resT, a, b]
    show (OpLogicalNot res resT x) = 
        showOpWithResult res "OpLogicalNot" [resT, x]
    show (OpTypeVoid res) = showOpWithResult res "OpTypeVoid" []
    show (OpTypeBool res) = showOpWithResult res "OpTypeBool" []
    show (OpTypeInt res width signed) = 
        show res ++ " = OpTypeInt " ++ show width ++ " " ++
        if signed then "1" else "0"
    show (OpTypeFloat res width) = show res ++ " = OpTypeFloat " ++ show width 
    show (OpTypeVector res t size) = 
        show res ++ " = OpTypeVector " ++ show t ++ " " ++ show size
    show (OpTypePointer res storage t) = 
        show res ++ " = OpTypePointer " ++ show storage ++ " " ++ show t
    show (OpTypeFunction res resT argTypes) = 
        showOpWithResult res "OpTypeFunction" (resT:argTypes)

showOpWithResult :: SpirId -> String -> [SpirId] -> String
showOpWithResult resId opName args = show resId ++ " = " ++ opName ++ 
    (if not (null args) then " " else "") ++ unwords (show <$> args)

instance Show SpirFunctionControl where
    show fc = case fc of
        FCNone -> "None"
        FCInline -> "Inline"
        FCDontInline -> "DontInline"
        FCPure -> "Pure"
        FCConst -> "Const"

instance Show SpirConst where
    show (SCFloat f) = show f
    show (SCBool b)  = show b
    show (SCUnsigned ui) = show ui

instance Show SpirStorageClass where
    show StorFunction = "Function"
