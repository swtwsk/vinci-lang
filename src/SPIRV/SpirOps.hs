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
            | OpLoopMerge SpirId SpirId SpirLoopControl
            | OpSelectionMerge SpirId SpirSelectionControl
            | OpReturn
            | OpReturnValue SpirId
            | OpFunctionEnd
            | OpFunctionCall SpirId SpirId SpirId [SpirId]
            | OpExtInst SpirId SpirId SpirId String [SpirId]
            | OpConstantTrue SpirId SpirId
            | OpConstantFalse SpirId SpirId
            | OpConstant SpirId SpirId SpirConst
            | OpSNegate SpirId SpirId SpirId
            | OpFNegate SpirId SpirId SpirId
            | OpIAdd SpirId SpirId SpirId SpirId
            | OpFAdd SpirId SpirId SpirId SpirId
            | OpISub SpirId SpirId SpirId SpirId
            | OpFSub SpirId SpirId SpirId SpirId
            | OpIMul SpirId SpirId SpirId SpirId
            | OpFMul SpirId SpirId SpirId SpirId
            | OpSDiv SpirId SpirId SpirId SpirId
            | OpFDiv SpirId SpirId SpirId SpirId
            | OpSMod SpirId SpirId SpirId SpirId
            | OpFMod SpirId SpirId SpirId SpirId
            | OpIEqual SpirId SpirId SpirId SpirId
            | OpFOrdEqual SpirId SpirId SpirId SpirId
            | OpSLessThan SpirId SpirId SpirId SpirId
            | OpFOrdLessThan SpirId SpirId SpirId SpirId
            | OpLogicalEqual SpirId SpirId SpirId SpirId
            | OpLogicalOr SpirId SpirId SpirId SpirId
            | OpLogicalAnd SpirId SpirId SpirId SpirId
            | OpLogicalNot SpirId SpirId SpirId

            | OpTypeVoid SpirId
            | OpTypeBool SpirId
            | OpTypeInt SpirId Int Bool
            | OpTypeFloat SpirId Int
            | OpTypeVector SpirId SpirId Int
            | OpTypeArray SpirId SpirId SpirId
            | OpTypeStruct SpirId [SpirId]
            | OpTypePointer SpirId SpirStorageClass SpirId
            | OpTypeFunction SpirId SpirId [SpirId]

            | OpCapability SpirCapability
            | OpExtInstImport SpirId String
            | OpDecorate SpirId SpirDecoration [Int]
            | OpMemberDecorate SpirId Int SpirDecoration [Either Int SpirBuiltIn]
            | OpEntryPoint SpirExecutionModel SpirId String [SpirId]
            | OpExecutionMode SpirId SpirExecutionMode
            | OpMemoryModel SpirAddressingModel SpirMemoryModel
            deriving Eq

data SpirConst = SCFloat Double
               | SCSigned Int
               | SCUnsigned Int
               deriving Eq

data SpirFunctionControl = FCNone | FCInline | FCDontInline | FCPure | FCConst
                         deriving Eq

data SpirStorageClass = StorFunction 
                      | StorInput
                      | StorOutput
                      | StorUniform
                      | StorUniformConstant
                      deriving (Eq, Ord) -- and other

data SpirLoopControl = LCNone | LCUnroll | LCDontUnroll deriving Eq -- and other

data SpirSelectionControl = SelCtrNone | SelCtrFlatten | SelCtrDontFlatten deriving Eq

data SpirDecoration = Block 
                    | Offset 
                    | Location 
                    | Binding 
                    | DescriptorSet
                    | BuiltIn
                    deriving (Eq, Show)

data SpirBuiltIn = Position
                 | PointSize
                 | ClipDistance
                 | CullDistance
                 deriving (Eq, Show)

data SpirExecutionModel = Vertex | Fragment deriving (Eq, Show)

data SpirExecutionMode = OriginUpperLeft | OriginLowerLeft deriving (Eq, Show)

data SpirCapability = Shader | VariablePointers deriving (Eq, Show)

data SpirAddressingModel = Logical deriving (Eq, Show)

data SpirMemoryModel = Simple | GLSL450 deriving (Eq, Show)

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
    show (OpLoopMerge mb ct loopCtr) = 
        "OpLoopMerge " ++ show mb ++ " " ++ show ct ++ " " ++ show loopCtr
    show (OpSelectionMerge mb selCtr) =
        "OpSelectionMerge " ++ show mb ++ " " ++ show selCtr
    show OpReturn = "OpReturn"
    show (OpReturnValue arg) = "OpReturnValue " ++ show arg
    show OpFunctionEnd = "OpFunctionEnd"
    show (OpFunctionCall res fType fName args) = 
        showOpWithResult res "OpFunctionCall" (fType:fName:args)
    show (OpExtInst res fType extId fName args) = show res ++ " = OpExtInst " ++ 
        show fType ++ " " ++ show extId ++ " " ++ fName ++ " " ++ 
        (if not (null args) then " " else "") ++ unwords (show <$> args)
    show (OpConstantTrue res resT)  = 
        show res ++ " = OpConstantTrue " ++ show resT
    show (OpConstantFalse res resT) = 
        show res ++ " = OpConstantFalse " ++ show resT
    show (OpConstant res resType c) = 
        show res ++ " = OpConstant " ++ show resType ++ " " ++ show c
    show (OpSNegate res resT x) = showOpWithResult res "OpSNegate" [resT, x]
    show (OpFNegate res resT x) = showOpWithResult res "OpFNegate" [resT, x]
    show (OpIAdd res resT a b) = showOpWithResult res "OpIAdd" [resT, a, b]
    show (OpFAdd res resT a b) = showOpWithResult res "OpFAdd" [resT, a, b]
    show (OpISub res resT a b) = showOpWithResult res "OpISub" [resT, a, b]
    show (OpFSub res resT a b) = showOpWithResult res "OpFSub" [resT, a, b]
    show (OpIMul res resT a b) = showOpWithResult res "OpIMul" [resT, a, b]
    show (OpFMul res resT a b) = showOpWithResult res "OpFMul" [resT, a, b]
    show (OpSDiv res resT a b) = showOpWithResult res "OpSDiv" [resT, a, b]
    show (OpFDiv res resT a b) = showOpWithResult res "OpFDiv" [resT, a, b]
    show (OpSMod res resT a b) = showOpWithResult res "OpSMod" [resT, a, b]
    show (OpFMod res resT a b) = showOpWithResult res "OpFMod" [resT, a, b]
    show (OpIEqual res resT a b) = showOpWithResult res "OpIEqual" [resT, a, b]
    show (OpFOrdEqual res resT a b) = 
        showOpWithResult res "OpFOrdEqual" [resT, a, b]
    show (OpSLessThan res resT a b) =
        showOpWithResult res "OpSLessThan" [resT, a, b]
    show (OpFOrdLessThan res resT a b) = 
        showOpWithResult res "OpFOrdLessThan" [resT, a, b]
    show (OpLogicalEqual res resT a b) = 
        showOpWithResult res "OpLogicalEqual" [resT, a, b]
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
    show (OpTypeArray res t size) =
        show res ++ " = OpTypeArray " ++ show t ++ " " ++ show size
    show (OpTypeStruct res fieldTypes) =
        showOpWithResult res "OpTypeStruct" fieldTypes
    show (OpTypePointer res storage t) = 
        show res ++ " = OpTypePointer " ++ show storage ++ " " ++ show t
    show (OpTypeFunction res resT argTypes) = 
        showOpWithResult res "OpTypeFunction" (resT:argTypes)
    show (OpCapability capability) = "OpCapability " ++ show capability
    show (OpExtInstImport res extendedInstructionSet) =
        show res ++ " = OpExtInstImport \"" ++ extendedInstructionSet ++ "\""
    show (OpDecorate targetId decoration decArgs) =
        "OpDecorate " ++ show targetId ++ " " ++ show decoration ++ " " ++
        (if null decArgs then "" else " ") ++ unwords (show <$> decArgs)
    show (OpMemberDecorate typeId memberId decoration decArgs) =
        "OpMemberDecorate " ++ show typeId ++ " " ++ show memberId ++ 
        " " ++ show decoration ++ (if null decArgs then "" else " ") ++ 
        unwords (either show show <$> decArgs)
    show (OpEntryPoint execModel entryPointId entryPointName args) =
        "OpEntryPoint " ++ show execModel ++ " " ++ show entryPointId ++ 
        " \"" ++ entryPointName ++ "\" " ++ unwords (show <$> args)
    show (OpExecutionMode entryPointId execMode) = 
        "OpExecutionMode " ++ show entryPointId ++ " " ++ show execMode
    show (OpMemoryModel addressing memory) =
        "OpMemoryModel " ++ show addressing ++ " " ++ show memory

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
    show (SCSigned i) = show i
    show (SCUnsigned ui) = show ui

instance Show SpirStorageClass where
    show StorFunction        = "Function"
    show StorInput           = "Input"
    show StorOutput          = "Output"
    show StorUniform         = "Uniform"
    show StorUniformConstant = "UniformConstant"

instance Show SpirLoopControl where
    show LCNone = "None"
    show LCUnroll = "Unroll"
    show LCDontUnroll = "DontUnroll"

instance Show SpirSelectionControl where
    show SelCtrNone = "None"
    show SelCtrFlatten = "Flatten"
    show SelCtrDontFlatten = "DontFlatten"
