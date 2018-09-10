{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Day16b where

import Data.List (foldl')
import Data.Map.Strict (Map(..))
import Data.String (IsString(..))
import Data.Semigroup (Semigroup(..), stimes)
import Data.Vector (Vector(..), fromList)

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Control.Monad (liftM2)
import Control.DeepSeq (($!!), (<$!!>), NFData)
import Text.Read (readMaybe)

import System.Random.Shuffle (shuffle')
import System.Random (getStdGen)
import qualified System.IO.Strict as S



data Step = Spin Int | Exchange Int Int | Partner Char Char deriving (Eq, Show)

class MRead a where
  readm :: String -> Maybe a

instance IsString (Vector Char) where
  fromString = fromList

instance MRead Int where
  readm = readMaybe

instance MRead Char where
  readm (x:[]) = Just x
  readm _ = Nothing

parseInstruction :: String -> Maybe Step
parseInstruction "" = Nothing
parseInstruction (s:ss) | s == 's' = Just . Spin . read $ ss
                        | s == 'x' = uncurry Exchange <$> parsepairs ss
                        | s == 'p' = uncurry Partner <$> parsepairs ss
                        | otherwise = Nothing
                          where
                            parsepairs :: MRead a => String -> Maybe (a, a)
                            parsepairs ss = (mapD readm <$> getpair ss) >>= sequenceD

getpair :: String -> Maybe (String, String)
getpair str = go str ""
  where
    go "" _ = Nothing
    go (x:xs) acc = if x == '/'
                    then Just (acc, xs)
                    else go xs (acc++[x])

-- map for doubles
mapD :: (a -> b) -> (a,a) -> (b,b)
mapD f (a,b) = (f a, f b)

sequenceD :: Monad m => (m a, m b) -> m (a, b)
sequenceD = uncurry $ liftM2 (,)

spin :: Int -> Vector Char -> Vector Char
spin n vec = (V.drop m vec) V.++ (V.take m vec)
  where
    m = V.length vec - (n `rem` V.length vec)

exchange :: Int -> Int -> Vector Char -> Vector Char
exchange n m vec = if any (>=length vec) (n,m)
                   then errorWithoutStackTrace "n|m >= length str in exchange"
                   else V.update vec (fromList [(n,b), (m,a)])
                        where
                          a = vec V.! n
                          b = vec V.! m

partner :: Char -> Char -> Vector Char -> Vector Char
partner a b vec = V.map f vec
  where
    f x | x == a    = b
        | x == b    = a
        | otherwise = x

accMapAt :: Eq a => a -> ([a] -> b) -> [a] -> [b]
accMapAt c f ls = go ls []
  where
    go [] [] = []
    go [] acc = f acc : []
    go (x:xs) acc | x == c    = f acc : go xs []
                  | otherwise = go xs (acc++[x])

class AppStep a where
  appStep :: Step -> a -> a

instance AppStep (Vector Char) where
  appStep (Spin x)       = spin x
  appStep (Exchange x y) = exchange x y
  appStep (Partner x y)  = partner x y

spin' :: Int -> [Int] -> [Int]
spin' n ls = fmap (\x -> (x+n) `rem` len) ls
  where
    len = length ls

swap :: Eq a => a -> a -> [a] -> [a]
swap a b ls = fmap ex ls
  where
    ex x | x == a    = b
         | x == b    = a
         | otherwise = x

instance AppStep (([Int], [Char])) where
  appStep (Spin x) (is, cs)       = (spin' x is, cs)
  appStep (Exchange x y) (is, cs) = (swap x y is, cs)
  appStep (Partner x y) (is, cs)  = (is, swap x y cs)

doPermDance :: (Map Int Int, Map Char Char) -> Vector Char -> Vector Char
doPermDance (itable, ctable) ps = fromList
                                  . fromMaybe "failed lookup"
                                  . sequence
                                  . map (>>= (ctable M.!?))
                                  $ go 0 (length itable) ps
  where
    go n m ps | n >= m = []
              | otherwise = p : go (n+1) m ps
              where
                p = (\x -> ps V.! x)
                    <$> (itable M.!? n)


mkPermDance size steps = let (is, cs) = foldl' (flip ($!!)) ([0..(size-1)], take size ['a'..]) steps
                             ctable = M.fromList (zip ['a'..] cs)
                             itable = M.fromList (zip is [0..]) -- result of transformation is key,
                                                                -- so we can do inverst lookup
                         in
                           (itable, ctable)
newtype Dance = Dance (Map Int Int, Map Char Char)
instance Semigroup Dance where
  (Dance (a1, a2)) <> (Dance (b1, b2)) = Dance (fuseMaps a1 b1, fuseMaps a2 b2)

programs :: Vector Char
programs = fromList ['a'..'p']
plen = V.length programs

getSteps :: IO [Step]
getSteps = fromMaybe "failed parsing"
            <$> sequence
            <$> accMapAt ',' parseInstruction
            <$> S.readFile "data/day16.txt"

main = do dance0 <- mkPermDance plen <$!!> map appStep <$> getSteps
          --runSeveral (round 100) (doPermDance dance0) programs >>= (print $!!)
          (print $!!) . (\d -> doPermDance d programs) . dotimes (1e9) $ dance0
            where
              dotimes n = (\case Dance x -> x) . stimes (round n) . Dance


runSeveral :: (NFData a, Integral i, Show i) => i -> (a -> a) -> a -> IO a
--runSeveral :: Control.DeepSeq.NFData a => Int -> (a -> a) -> a -> IO a
runSeveral 0 _ arg = return arg
runSeveral n f arg | n `rem` 1000000 == 0 = print n
                                         >> (runSeveral (n-1) f $!! (f arg))
                   | otherwise = runSeveral (n-1) f (f arg)

fromMaybe :: String -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe msg Nothing = errorWithoutStackTrace msg

-- test
testSteps = shuffle' ls (length ls) <$> getStdGen
  where ls = fromMaybe "failed parsing"
             $ sequence
             $ accMapAt ',' parseInstruction
             $ "s1,s4,pa/b,pc/d,x1/2,x0/2"

doDance :: [(Vector Char -> Vector Char)] -> Vector Char -> Vector Char
doDance dances progs = foldl' (flip ($!!)) progs dances

test :: IO ()
test = do dance <- testSteps
          --putStrLn "Spins:"
          --print $ doDance (f' $ filter (\case { (Spin _) -> True; _ -> False}) dance) progs
          --print $ doPermDance (f $ filter (\case { (Spin _) -> True; _ -> False}) dance) progs
          --putStrLn "Partner:"
          --print $ doDance (f' $ filter (\case { (Partner _ _) -> True; _ -> False}) dance) progs
          --print $ doPermDance (f $ filter (\case { (Partner _ _) -> True; _ -> False}) dance) progs
          --putStrLn "Exchange:"
          --print $ doDance (f' $ filter (\case { (Exchange _ _) -> True; _ -> False}) dance) progs
          --print $ doPermDance (f $ filter (\case { (Exchange _ _) -> True; _ -> False}) dance) progs
          print $ doDance (f' dance) progs
          runSeveral 1 (doDance (f' dance)) progs >>= print
          print $ doPermDance (f dance) progs
            where
              progs = fromList "abcde"
              f' ds = map appStep ds
              f ds = mkPermDance (length ds) $ map appStep ds
              sortedSteps dances = filter (not' isPartner) dances ++ filter isPartner dances
                                       where
                                         isPartner (Partner _ _) = True
                                         isPartner _             = False
                                         not' f = \x -> not (f x)

fuseMaps :: Ord k => Map k a -> Map b k -> Map b a
fuseMaps a b = (a M.!) <$> b
