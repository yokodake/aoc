module Day19 where

import Data.Vector (Vector(..), fromList, (!), (!?), elemIndex)
import qualified Data.Vector as V


type Diagram = Vector (Vector Char)

diagram :: IO Diagram
diagram = (fromList . fmap fromList) <$> puzzleInput

testInput, puzzleInput :: IO [String]
testInput = return i
  where
    i =
      [ "     |         "
      , "     |  +--+   "
      , "     A  |  C   "
      , " F---|----E|--+"
      , "     |  |  |  D"
      , "     +B-+  +--+"
      ]
puzzleInput = lines <$> readFile "data/day19.txt"

data Move = V Int | H Int deriving (Show, Eq)
right, left, up, down :: Move
right = H 1
left  = H (-1)
up    = V (-1)
down  = V 1

begin :: Diagram -> Int
begin puz = case elemIndex '|' (puz ! 0) of
              Just i -> i
              Nothing -> errorWithoutStackTrace "couldn't find begin"

(<!>) :: Diagram -> (Int, Int) -> Char
d <!> (r,c) = d ! r ! c
(<!?>) :: Diagram -> (Int, Int) -> Maybe Char
d <!?> (r,c) = d !? r >>= (!? c)

fromuntil :: Diagram -> (Int, Int) -> Move -> (String, Maybe (Int, Int))
fromuntil diag src mv = go "" (next src)
  where
    go acc pos | p == '+'  = (acc, Just pos)
               | p == ' '  = (acc, Nothing)
               | isPipe p  = go acc (next pos)
               | otherwise = go (acc++[p]) (next pos)
                 where
                   p = diag <!> pos
    isPipe c = (c=='|') || (c=='-')
    next = move mv

fromuntilInc :: Diagram -> (Int, Int) -> Move -> (Int, Maybe (Int, Int))
fromuntilInc diag src mv = go 0 (next src)
  where
    go acc pos | p == '+' = (acc, Just pos)
               | p == ' ' = (acc, Nothing)
               | otherwise = go (acc+1) (next pos)
                 where
                   p = diag <!> pos
    next = move mv

move :: Move -> (Int, Int) -> (Int, Int)
move (V n) (x,y) = (x+n, y)
move (H n) (x,y) = (x, y+n)

chdir :: Diagram -> (Int, Int) -> Move -> Move
chdir d p dir = ndir $ getN adjacents odirchar
  where
    adjacents = case dir of
      V _ -> (move left p, move right p)
      H _ -> (move up p, move down p)

    (ndir, odirchar) = case dir of
                         V _ -> (H, '|')
                         H _ -> (V, '-')

    getN (p1, p2) c =
      let
        (c1, c2) = (d <!?> p1, d <!?>p2)
        isC = fmap (\x -> x/=' ' && x/=c)
      in
      case (isC c1, isC c2) of
        (Nothing, Nothing) -> errorWithoutStackTrace "Both adjacents are out of range"
        (Just False, Just False) -> errorWithoutStackTrace ("No pipe @("++show p1++","++show p2++")")
        (Just True, Just True)  -> errorWithoutStackTrace ("fork @"++show p)
        (Just True, _) -> -1
        (_, Just True) -> 1

-- LAZY ASFCUK AND JUST COPY-WRITED THIS SHIT INSTEAD OF FINDING A NICE ABSTRACTION
run diag = go (0, begin diag) (V 1) ""
  where
    go start mv acc = let (str, pos) = fromuntil diag start mv
                      in
                        case pos of
                          Nothing -> acc++str
                          Just p -> go p (chdir diag p mv) (acc++str)

run2 diag = go (0, begin diag) (V 1) 0
  where
    go start mv acc = let (i, pos) = fromuntilInc diag start mv
                      in
                        case pos of
                          Nothing -> acc+i+1
                          Just p -> go p (chdir diag p mv) (acc+i+1)

main = diagram >>= (print . run2)
