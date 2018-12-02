{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Day20 where

import Control.DeepSeq (NFData, force)
import Data.Foldable (sum)
import Data.Map(Map, (!), empty, insertWith)
import Data.Vector (Vector(..), fromList)
import Data.Void (Void)
import GHC.Generics (Generic)
import Linear (V3(..))
import Text.Megaparsec (Parsec, parseMaybe)
import Text.Megaparsec.Char (string)
import qualified Data.Vector as V
import qualified Text.Megaparsec as T
import qualified Text.Megaparsec.Char.Lexer as L


main :: IO ()
main = particles >>=
       -- print . part1
       print . part2 100

part2 :: Int -> Buffer -> Int
part2 end buffer = go 0 (length buffer) buffer
  where
    go :: Int -> Int -> Buffer -> Int
    go n len buf | n > end = len
                 | otherwise =
                   let
                     new = collide (force buf)
                     newlen = length new
                     m = if newlen == len then succ n else 0
                   in
                     go m newlen (V.map step new)

part1 :: Buffer -> Int
part1 = V.minIndex . V.map normish

type Triple = V3 Int
type Buffer = Vector (Particle Triple)

data Particle a = Particle { acc :: !a
                           , vel :: !a
                           , pos :: !a
                           }
                deriving (Show, Eq, Functor, Foldable, Ord, Generic, NFData)

particles :: IO Buffer
particles = fromList . fromMaybe . sequence . map (parseMaybe particleP)
            <$> puzzleInput
  where
    fromMaybe Nothing = errorWithoutStackTrace "error parsing particles"
    fromMaybe (Just x) = x

testInput, puzzleInput :: IO [String]
testInput = return $
  [ "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>"
  , "p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>"
  ]

puzzleInput = lines <$> readFile "data/day20.txt"

normish :: Particle Triple -> Particle Int
normish = fmap (sum . abs)

collide :: Buffer -> Buffer
collide buffer = filtCollisions findCollisions
  where
    findCollisions :: Map (V3 Int) Int
    findCollisions = V.foldr (\x -> insertWith (+) (pos x) 1) empty buffer
    filtCollisions ps = V.filter (\x -> ps ! (pos x) < 2) buffer

step (Particle a v p) = force $ Particle a (v + a) (p + (v + a))



-- parser
type Parser = Parsec Void String

intP :: Parser Int
intP = signed
           where
             signed = L.signed sc L.decimal

sc :: Parser ()
sc = L.space T.empty T.empty T.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

tripleP :: Parser Triple
tripleP = do string "<"
             x1 <- intP
             string ","
             x2 <- intP
             string ","
             x3 <- intP
             string ">"
             return $ V3 x1 x2 x3

particleP :: Parser (Particle Triple)
particleP = do string "p="
               p <- tripleP
               string ", v="
               v <- tripleP
               string ", a="
               a <- tripleP
               return $ Particle a v p
