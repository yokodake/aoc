#! /usr/bin/env stack
-- stack --resolver lts-11.14 script

import Test.Hspec
import Text.Read (readMaybe)
import Control.Monad (liftM2)
import qualified System.IO.Strict as S

data Dance = Spin Int | Exchange Int Int | Partner Char Char deriving (Eq, Show)

class MRead a where
  readm :: String -> Maybe a

instance MRead Int where
  readm = readMaybe

instance MRead Char where
  readm (x:[]) = Just x
  readm _ = Nothing


parseInstruction :: String -> Maybe Dance
parseInstruction "" = Nothing
parseInstruction (s:ss) | s == 's' = Just . Spin . read $ ss
                        | s == 'x' = uncurry Exchange <$> parsepairs ss
                        | s == 'p' = uncurry Partner <$> parsepairs ss
                        | otherwise = Nothing
                          where
                            parsepairs :: MRead a => String -> Maybe (a, a)
                            parsepairs ss = (mapT readm <$> getpair ss) >>= dsequence

getpair :: String -> Maybe (String, String)
getpair str = go str ""
  where
    go "" _ = Nothing
    go (x:xs) acc = if x == '/'
                    then Just (acc, xs)
                    else go xs (acc++[x])

mapT :: (a -> b) -> (a,a) -> (b,b)
mapT f (a,b) = (f a , f b)

dsequence :: Monad m => (m a, m b) -> m (a, b)
dsequence = uncurry $ liftM2 (,) -- don't know how to name that though

spin :: Int -> String -> String
spin n str = (drop m str) ++ (take m str)
  where
    m = length str - (n `rem` length str)

exchange :: Int -> Int -> String -> String
exchange n m str = if any (>=length str) (n,m)
                   then errorWithoutStackTrace "n|m >= length str in exchange"
                   else partner a b str
                        where
                          a = str !! n
                          b = str !! m

partner :: Char -> Char -> String -> String
partner a b str = go str
  where
    go "" = ""
    go (x:xs) | x == a    = b : go xs
              | x == b    = a : go xs
              | otherwise = x : go xs

applyDance :: String -> String -> String
applyDance str = case (parseInstruction str) of
  Just (Spin x) -> spin x
  Just (Exchange x y) -> exchange x y
  Just (Partner x y) -> partner x y
  Nothing -> errorWithoutStackTrace $ "failed parsing " ++ str

accMapAt :: Eq a => a -> ([a] -> b) -> [a] -> [b]
accMapAt c f ls = go ls []
  where
    go [] [] = []
    go [] acc = f acc : []
    go (x:xs) acc | x == c    = f acc : go xs []
                  | otherwise = go xs (acc++[x])

-- compose it all together
run :: String -> String -> String
run progs ins = foldl f progs $ accMapAt ',' applyDance ins
  where f acc dance = dance acc

programs = ['a'..'p']

main = do utests
          S.readFile "input/day1.txt"
            >>= print . run programs

utests = hspec $ do
  context "funcs" $ do
    specify "spin" $ do
      spin 1 "abcde" `shouldBe` "eabcd"
      spin 3 "abcde" `shouldBe` "cdeab"
      spin 5 "abcde" `shouldBe` "abcde"
      spin 6 "abcde" `shouldBe` "eabcd"
    specify "exchange" $ do
      exchange 0 1 "abcde" `shouldBe` "bacde"
      exchange 0 4 "abcde" `shouldBe` "ebcda"
      exchange 2 2 "abcde" `shouldBe` "abcde"
      exchange 4 1 "abcde" `shouldBe` "aecdb"
    specify "partner" $ do
      partner 'a' 'b' "abcde" `shouldBe` "bacde"
      partner 'a' 'e' "abcde" `shouldBe` "ebcda"
      partner 'e' 'a' "abcde" `shouldBe` "ebcda"
      partner 'c' 'c' "abcde" `shouldBe` "abcde"
  context "parsing" $ do
    specify "spin" $ do
      parseInstruction "s1"  `shouldBe` Just (Spin 1)
      parseInstruction "s15" `shouldBe` Just (Spin 15)
    specify "exchange" $ do
      parseInstruction "x3/4"   `shouldBe` Just (Exchange 3 4)
      parseInstruction "x11/3"  `shouldBe` Just (Exchange 11 3)
      parseInstruction "x3/11"  `shouldBe` Just (Exchange 3 11)
      parseInstruction "x11/11" `shouldBe` Just (Exchange 11 11)
    specify "partner" $ do
      parseInstruction "pe/b"   `shouldBe` Just (Partner 'e' 'b')
  context "helpers" $ do
    specify "accMapAt" $ do
      accMapAt 0 id [1,2,0,3,4,0] `shouldBe` [[1,2],[3,4]]
      accMapAt 0 id [0,0]         `shouldBe` [[],[]]
      accMapAt 0 (any (==2)) [1,2,0,3,4,0] `shouldBe` [True,False]
      accMapAt ':' length "abcd:ab:abc" `shouldBe` [4,2,3]
  context "example_blocks" $ do
    testIns <- return "s1,x3/4,pe/b"
    specify "parsing" $ do
      sequence (accMapAt ',' parseInstruction testIns) `shouldBe` Just [Spin 1, Exchange 3 4, Partner 'e' 'b']
    specify "example test2" $ do
      run "abcde" testIns `shouldBe` "baedc"
