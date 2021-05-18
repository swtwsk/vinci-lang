module Utils.Tuple where

fstTriple :: (a, b, c) -> a
fstTriple (a, _, _) = a

sndTriple :: (a, b, c) -> b
sndTriple (_, b, _) = b

trdTriple :: (a, b, c) -> c
trdTriple (_, _, c) = c
