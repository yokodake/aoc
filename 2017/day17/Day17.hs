module Day17 where

import Data.Vector (Vector(..), fromList)
import qualified Data.Vector as V

main = let (i, vec) = dotimes 2017 (spinlock 380) (0, start)
       in
         do print $ vec V.! (i+1)
            print $ vec V.! 1
            print $ spinlockI 380 0 (0, 1) 2017
            print $! spinlockI 380 0 (0, 1) (round 5e7)


start = V.singleton 0

spinlock :: Int -> (Int, Vector Int) -> (Int, Vector Int)
spinlock steps (i, buffer) =
  let
    len = V.length buffer
    j = (i+steps) `rem` len + 1
    (hd, tl) = V.splitAt j buffer
  in
    (j, hd V.++ (V.cons len tl))

dotimes 0 _ r = r
dotimes n f r = dotimes (n-1) f (f r)

spinlockI steps value (i, size) max | size > max = value
                                    | otherwise  = value `seq` i `seq` spinlockI steps v (j+1, size+1) max
                                      where
                                        j = (i+steps) `rem` size
                                        v | j == 0    = size
                                          | otherwise = value
