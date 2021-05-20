module Utils.Tuple where

fstTriple :: (a, b, c) -> a
fstTriple (a, _, _) = a

sndTriple :: (a, b, c) -> b
sndTriple (_, b, _) = b

trdTriple :: (a, b, c) -> c
trdTriple (_, _, c) = c

firstTriple :: (a -> d) -> (a, b, c) -> (d, b, c)
firstTriple f (a, b, c) = (f a, b, c)

secondTriple :: (b -> d) -> (a, b, c) -> (a, d, c)
secondTriple f (a, b, c) = (a, f b, c)

thirdTriple :: (c -> d) -> (a, b, c) -> (a, b, d)
thirdTriple f (a, b, c) = (a, b, f c)
