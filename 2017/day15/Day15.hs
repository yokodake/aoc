{-# LANGUAGE DeriveGeneric #-}
module Day15 where

import Data.List (foldl')
import Control.DeepSeq
import GHC.Generics
import Data.Bits

newtype Nint = Nint Integer deriving (Show, Generic, Ord)
instance Eq Nint where
  (==) (Nint a) (Nint b) = l16bits a == l16bits b
    where
      l16bits = (.&. 0xffff)

instance NFData Nint -- laziness fucks us over in the for loop


next :: Integer -> Integer -> Nint -> Nint
next mul cond (Nint pred) = if res `rem` cond == 0
                            then r
                            else next mul cond $!! r
                              where
                                cstdiv = 2147483647
                                r@(Nint res) = Nint ((pred * mul) `rem` cstdiv)

nexta = next 16807 4
nextb = next 48271 8
testa = nexta $ Nint 65
testb = nextb $ Nint 8921
starta = nexta $ Nint 703
startb = nextb $ Nint 516

-- the first way I thought about doing this
for :: NFData a => (a -> a) -> a -> Integer -> a
for f acc 0 = acc
for f acc n = for f (f $!! acc) (n-1)

judge :: (Integer, Nint, Nint) -> (Integer, Nint, Nint)
judge (acc, x, y) =
  let
    newx = nexta x
    newy = nextb y
    n = if newx == newy then 1 else 0
  in
    (acc+n, newx, newy)

run1 times start = fst3 $! for judge start times
main1 = print $ run1 40000000 (0, starta, startb)

run2 times start = fst3 $! iterate (judge$!!) start !! times
main2 = print $ run2 5000000 (0, starta, startb)

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a
