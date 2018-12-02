{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Day18 where

import Data.Tuple (swap)
import Data.Map (Map(..), adjust)
import Data.Vector (Vector(..))
import Data.Void (Void(..))
import qualified Data.Map as M
import qualified Data.Vector as V

import GHC.Generics (Generic(..))
import Control.DeepSeq(($!!), NFData(..), deepseq, force)
import Control.Monad (void)
import Control.Monad.State.Strict (State(..), state, runState)
import qualified Control.Monad.State.Strict as ST

import System.IO.Strict as S

import Text.Megaparsec (Parsec(..), some, (<|>), try, parseMaybe, parseTest)
import Text.Megaparsec.Char (lowerChar, spaceChar, space1, space,
                             string, alphaNumChar)
import qualified Text.Megaparsec as T
import qualified Text.Megaparsec.Char.Lexer as L

data Arg = R Char
         | L Int
         deriving (Show, Generic)

instance NFData Arg


newtype Reg = Reg { reg :: Char}
            deriving (Show, Generic)
instance NFData Reg

data Instruction = Snd Arg
                 | Rcv Reg
                 | Set Reg Arg
                 | Add Reg Arg
                 | Mul Reg Arg
                 | Mod Reg Arg
                 | Jgz Arg Arg
                 deriving (Show, Generic)

instance NFData Instruction

data Registers = Registers { getQ :: Queue Int,
                             getR :: Map Char Int } deriving (Show)

rmap :: (Map Char Int -> Map Char Int) -> Registers -> Registers
rmap f (Registers q r) = Registers q $ f r
qmap :: (Queue Int -> Queue Int) -> Registers -> Registers
qmap f (Registers q r) = Registers (f q) r

ipr, snr, cnr :: Char
ipr = 'I' -- instruction pointer
snr = 'S' -- value to send
cnr = 'C' -- # values sent

incip :: Registers -> Registers
incip = rmap $ adjust (+1) ipr

lookupWithDefault :: Ord k => k -> a -> Map k a -> a
lookupWithDefault k a = (\case {Just x -> x; Nothing -> a}) . M.lookup k

trInstruction :: Instruction -> Registers -> Registers
trInstruction ins qr@(Registers q r) = incip . itof $ ins
  where
    itof :: Instruction -> Registers
    itof (Snd arg)         = error "not supposed to happen"
    itof (Rcv (Reg c))     = insertQ c $ pop q
    itof (Set (Reg c) arg) = inserti arg c r
    itof (Add (Reg c) arg) = alteri (+) arg c r
    itof (Mul (Reg c) arg) = alteri (*) arg c r
    itof (Mod (Reg c) arg) = alteri (flip mod) arg c r
    itof (Jgz cond arg) | 0 >= (getvalue cond r) = qr
                        | otherwise = adjust ((+) $ (getvalue arg r)-1) 'I' `rmap` qr

    insertQ _ (Nothing, _) = Registers q $ adjust (\x-> x-1) ipr r
    insertQ c (Just a, q') = Registers q'$ M.insert c a r
    inserti a c r = Registers q $ M.insert c (getvalue a r) r
    alteri f a c r = Registers q $ M.alter (fWithDef (f $ getvalue a r)) c r

getvalue :: Arg -> Map Char Int -> Int
getvalue (R c) = lookupWithDefault c 0
getvalue (L i) = const i

fWithDef f (Just x) = Just $ f x
fWithDef f Nothing  = Just $ f 0

cycleI :: Vector Instruction -> Registers -> (Maybe Int, Registers)
cycleI is r = let ci = is V.! (getR r M.! ipr)
                  s = case ci of
                    Rcv _ -> Just (getR r M.! snr)
                    _ -> Nothing
              in (s, trInstruction ci r)

testCycle :: Vector Instruction -> State Registers (Maybe Int)
testCycle v = state (cycleI v)

cycleB :: Vector Instruction -> State (Registers, Registers) (Maybe Int)
cycleB v = state (cycleIb v)

-- spaghetti code
cycleIb :: Vector Instruction -> (Registers, Registers) -> (Maybe Int, (Registers, Registers))
cycleIb is ps@(p0@(Registers q0 r0), p1@(Registers q1 r1)) =
  let i0 = is V.! (r0 M.! ipr)
      i1 = is V.! (r1 M.! ipr)
      (p1', p0') = uncurry (sendInstruction i1) . swap $ sendInstruction i0 p0 p1
  in
    case (q0, q1) of
      (Waiting, Waiting) -> (Just (r1 M.! cnr), ps)
      (_, Waiting) -> (Nothing, sendInstruction i0 p0 p1)
      (Waiting, _) -> (Nothing, swap $ sendInstruction i1 p1 p0)
      _ -> (Nothing, (p0', p1'))

sendInstruction :: Instruction -> Registers -> Registers -> (Registers, Registers)
sendInstruction (Snd a) src dst = let
                                    dst' = push (getvalue a $ getR src) `qmap` dst
                                    src' = incip . rmap (adjust (+1) cnr) $ src -- increment source and send-count
                                  in
                                    (src', dst')
sendInstruction i src dst = (trInstruction i src, dst)




-- parser
type Parser = Parsec Void String

argP :: Parser Arg
argP = intAP
       <|> (R <$> charP)

regP :: Parser Reg
regP = Reg <$> charP

charP :: Parser Char
charP = lowerChar

intAP :: Parser Arg
intAP = L <$> signed
             where
               signed :: Parser Int
               signed = L.signed sc L.decimal

sc :: Parser ()
sc = L.space T.empty T.empty T.empty


lexeme :: NFData a => Parser a -> Parser a
lexeme = L.lexeme sc

rword :: String -> Parser ()
rword w = w `deepseq` (lexeme . try) (string w *> T.notFollowedBy alphaNumChar)

sndP :: Parser Instruction
sndP = do string "snd"
          _ <- space1
          Snd <$> argP


rcvP :: Parser Instruction
rcvP = do string "rcv"
          _ <- space1
          Rcv <$> regP

opP :: Parser Instruction
opP = do ins <- ((string "add" >> return Add)
                  <|> (string "mul" >> return Mul)
                  <|> (string "mod" >> return Mod)
                  <|> (string "set" >> return Set))
         space1
         r <- regP
         space1
         a <- argP
         ins `seq` return $ ins r a

jgzP :: Parser Instruction
jgzP = do string "jgz"
          space1
          a1 <- argP
          space1
          a2 <- argP
          return $ Jgz a1 a2

insP :: Parser Instruction
insP = (opP
        <|> jgzP
        <|> sndP
        <|> rcvP)


initReg n = Registers (Queue []) (M.fromList [('I',0),('S',0),('p',n),('C',0)])
d18 v = go (initReg 0, initReg 1)
  where
    go s = s `seq` case runState (cycleB v) s of
      (Just x, _) -> print $ "result: "++ show x
      (Nothing, s1) -> print s1
                       >> go s1


main :: IO ()
main = let
         instructions = sequence . V.fromList . map (parseMaybe insP) . lines
                        <$> S.readFile "data/day18.txt"
       in
         instructions >>= \(Just x) -> d18 x

test :: IO ()
test = let
         (Just instructions) = sequence . V.fromList . map (parseMaybe insP)
                               $ testinput
       in
         do mapM_ print instructions
            putStrLn "--------------"
            d18 instructions


testinput = [ "set a 1"
            , "add a 2"
            , "mul a a"
            , "mod a 5"
            , "snd a"
            , "set a 0"
            , "rcv a"
            , "jgz a -1"
            , "set a 1"
            , "jgz a -2"
            ] -- thank you based macros


data Queue a = Queue {queue:: [a]}
             | Waiting
             deriving (Show)

push :: a -> Queue a -> Queue a
push a Waiting = Queue [a]
push a (Queue s) = Queue . (++[a]) $ s

pop :: Queue a -> (Maybe a, Queue a)
pop Waiting = (Nothing, Waiting)
pop (Queue []) = (Nothing, Waiting)
pop (Queue (x:[])) = (Just x, Waiting)
pop (Queue (x:xs)) = (Just x, Queue xs)
