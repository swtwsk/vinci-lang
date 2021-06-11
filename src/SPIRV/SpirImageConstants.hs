module SPIRV.SpirImageConstants where

data SpirImageDim = OneD | TwoD | ThreeD deriving (Eq, Ord)

data SpirImageDepth = NotDepth | Depth | NoDepthIndication deriving (Eq, Ord)

data SpirImageArrayed = NonArrayed | Arrayed deriving (Eq, Ord)

data SpirImageMultisampled = SingleSampled | MultiSampled deriving (Eq, Ord)

data SpirImageSampled = RunTimeSampled
                      | WithSampler
                      | WithoutSampler
                      deriving (Eq, Ord)

data SpirImageFormat = Unknown deriving (Eq, Ord, Show)

instance Show SpirImageDim where
    show OneD = "1D"
    show TwoD = "2D"
    show ThreeD = "3D"

instance Show SpirImageDepth where
    show NotDepth = "0"
    show Depth = "1"
    show NoDepthIndication = "2"

instance Show SpirImageArrayed where
    show NonArrayed = "0"
    show Arrayed = "1"

instance Show SpirImageMultisampled where
    show SingleSampled = "0"
    show MultiSampled = "1"

instance Show SpirImageSampled where
    show RunTimeSampled = "0"
    show WithSampler = "1"
    show WithoutSampler = "2"
