{-# LANGUAGE DeriveGeneric #-}
module Main where

import Numeric (showHex)
import Data.List (foldl')
import Control.DeepSeq
import GHC.Generics

newtype Nint = Nint Integer deriving (Show, Generic, Ord)
instance Eq Nint where
  (==) (Nint a) (Nint b) = l16bits a == l16bits b

instance NFData Nint -- laziness fucks us over in the for loop

l16bits x = (.&. 0xffff)

cstdiv = 2147483647
next :: Integer -> Integer -> Nint -> Nint
next mul cond (Nint pred) = if res `rem` cond == 0
                            then r
                            else next mul cond $!! r
                              where r@(Nint res) = Nint ((pred * mul) `rem` cstdiv)
nexta = next 16807 4
nextb = next 48271 8
testa = nexta $ Nint 65
testb = nextb $ Nint 8921
starta = nexta $ Nint 703
startb = nextb $ Nint 516

-- the first way I thought about this
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

main :: IO ()
main = print $ run2 5000000 (0, starta, startb)
test = print $ run1 1057 (0, testa, testb)

run1 times start = fst3 $! for judge start times
run2 times start = fst3 $! iterate (judge$!!) start !! times

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a
