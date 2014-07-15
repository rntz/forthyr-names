module Gen4 where

import Prelude hiding (filter)

import Debug.Trace (trace, traceStack)

import Control.Applicative
import Control.Exception (assert)
import Control.Monad
import Control.Monad.State.Strict (State, get, put, state, runState)

import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Traversable
import qualified Data.Map as Map

import System.Random (RandomGen, Random, StdGen)
import qualified System.Random as R

-- Utilities
isProb p = 0 <= p && p <= 1.0

replicateA i p = sequenceA (replicate i p)

endsWith :: String -> String -> Bool
endsWith ending string =
    ending == reverse (take (length ending) $ reverse string)

startsWith begin string = begin == take (length begin) string

chooseFrom :: (Num w, Ord w) => w -> [(w, a)] -> a
chooseFrom p ((w,x):xs) | p <= w = x
                        | otherwise = chooseFrom (p-w) xs
chooseFrom _ [] = error "out of choices"


-- Frequency distributions, for small finite sets of possibilities
newtype Freq a = Freq { freqMap :: Map a Rational }
    deriving (Show, Eq, Ord)

freqs :: Ord a => [(Rational, a)] -> Freq a
freqs l | total > 0 = Freq $ Map.fromListWith (+) [(x,w/total) | (w,x) <- l]
        | otherwise = error "invalid frequency distribution"
    where total = sum $ map fst l

freqList :: Freq a -> [(Rational, a)]
freqList (Freq m) = [(w,x) | (x,w) <- Map.toList m]

joinFreqs :: Ord a => Freq (Freq a) -> Freq a
joinFreqs ff = freqs [(w1*w2, x) | (w1,f) <- freqList ff, (w2,x) <- freqList f]

merge :: Ord a => [(Rational, Freq a)] -> Freq a
merge l = freqs [(w1*w2, x) | (w1,f) <- l, (w2,x) <- freqList f]

mapFreqs :: Ord b => (a -> b) -> Freq a -> Freq b
mapFreqs f dist = freqs [(w, f x) | (w,x) <- freqList dist]

filter :: Ord a => (a -> Bool) -> Freq a -> Freq a
filter p (Freq m) | total > 0 = Freq (Map.map (/ total) news)
                  | otherwise = error "filtered out all elements!"
    where news = Map.filterWithKey (\k _ -> p k) m
          total = sum $ map snd $ Map.toList news

-- Uses a double in the range [0,1] (inclusive or exclusive, no matter) to
-- sample from a distribution.
sampleWith :: Double -> Freq a -> a
sampleWith p dist = chooseFrom (toRational p) (freqList dist)


-- Random sampler monad
type Sampler g a = State g a
type G a = Sampler StdGen a

runIO :: G a -> IO a
runIO k = R.getStdRandom (runState k)

random :: (RandomGen g, Random a) => Sampler g a
random = state R.random

randomR :: (RandomGen g, Random a) => (a,a) -> Sampler g a
randomR range = state (R.randomR range)

-- Sample from a distribution
sample :: RandomGen g => Freq a -> Sampler g a
sample dist = do p <- random
                 return $ sampleWith p dist


-- Useful samplers
choice :: [(Double, G a)] -> G a
choice cs = do p <- randomR (0, total)
               chooseFrom p cs
  where total = sum $ map fst cs

-- weighted :: [(Double, a)] -> G a
-- weighted l = choice [(w,pure x) | (w,x) <- l]

heads :: Double -> G a -> G a -> G a
heads p x y = assert (isProb p) $ choice [(p, x), (1-p, y)]
tails p x y = heads p y x

uniform :: [G a] -> G a
uniform l = choice [(1,x) | x <- l]


-- Probability parameters
pYModified = 0.12          -- probability of y-modified vowel
pYIsPre = 0.7              -- probability that y is a pre- not post-modification
pPostR = 0.15              -- probability of r-post-modified vowel

pStopApproximant = 0.1          -- p. of approximant after a stop
pFricativeApproximant = 0.05    -- p. of approximant after a fricative
pLW = 0.1                       -- p. of "w" after an "l"


-- Some semantic tests
isStop x = x `elem` words "t k p n"
isFricative x = x `elem` words "f s sr x θ lh"
isAffricate x = x `elem` words "ts tsr tθ tlh kx"
isApproximant x = x `elem` words "l w r h hw"
isCompound x = x `elem` words "sr lh ts tsr tθ tlh kx"

isRepeat :: String -> String -> Bool
isRepeat a b = a == b || startsWith a b

needsVowel :: String -> Bool
needsVowel x = endsWith "w" x
               || endsWith "h" x
               || (endsWith "r" x && not (isVowel x))

-- Determining legitimacy of next phonemes
allowedAfter :: [String] -> String -> Bool
allowedAfter l v | isVowel v = not (null l || isVowel (head l))
allowedAfter [] c = c /= "r"
allowedAfter (prev:_) c =
    not (needsVowel prev
         || isRepeat prev c     -- no repeats
         -- no (stops or fricatives) followed by stops
         || (isStop c && (isStop prev || isFricative prev))
         -- x cannot be followed by stops or fricatives
         || (endsWith "x" prev && (isStop c || isFricative c))
         -- compounds are single phonemes, no splitting them up
         || isCompound (prev ++ c)
         -- consonantal r needs to come after a consonant
         || (c == "r" && isVowel prev))

-- UGH.
isVowel [x] = x `elem` "aeiou"
isVowel (x:"r") = x `elem` "aeiou"
isVowel (x:"y") = x `elem` "aeiou"
isVowel (x:"yr") = x `elem` "aeiou"
isVowel ('r':[x]) = x `elem` "aeiou"
isVowel ('r':x:"r") = x `elem` "aeiou"
isVowel ('r':x:"y") = x `elem` "aeiou"
isVowel ('r':x:"yr") = x `elem` "aeiou"
isVowel ('y':[x]) = x `elem` "aeiou"
isVowel ('y':x:"r") = x `elem` "aeiou"
isVowel ('y':x:"y") = x `elem` "aeiou"
isVowel ('y':x:"yr") = x `elem` "aeiou"
isVowel ('r':'y':[x]) = x `elem` "aeiou"
isVowel ('r':'y':x:"r") = x `elem` "aeiou"
isVowel ('r':'y':x:"y") = x `elem` "aeiou"
isVowel ('r':'y':x:"yr") = x `elem` "aeiou"
isVowel _ = False


-- Some frequency distributions
vowels = freqs [(3,"a"), (1,"e"), (0.7,"i"), (1.4,"o"), (0.25, "u")]

consonants = merge [(5, stops),
                    (2, fricatives),
                    (1, affricates),
                    (3, approximants)]

stops = freqs [(10, "t"), (6, "k"), (2, "p"), (8, "n")]
fricatives = freqs [(1,"f"), (1,"s"), (1,"sr"), (1,"x"), (1,"θ"), (1,"lh")]
affricates = freqs [(1,"ts"), (1,"tsr"), (1,"tθ"), (1,"tlh"), (1,"kx")]
approximants = freqs [(1,"l"), (1,"w"), (1,"r"), (1, "h"), (1, "hw")]

after :: [String] -> Freq String -> Freq String
after ctx f = filter (allowedAfter ctx) f


-- Basic generation
gen :: [String] -> Freq String -> G [String]
gen prev f = (:prev) <$> sample (after prev f)

syllable :: [String] -> G [String]
syllable prev = do prev <- consonant prev
                   prev <- postNucleus prev
                   prev <- vowel prev
                   postVowel prev

consonant (x:_) | needsVowel x = error ("needs vowel: " ++ show x)
consonant prev = gen prev consonants

postNucleus prev@(c:_) | needsVowel c = return prev
postNucleus prev@(c:_)
    | isStop c = tails pStopApproximant (pure prev) $ gen prev $
                 filter (`elem` words "l w r") approximants
    | isFricative c = tails pFricativeApproximant (pure prev) $ gen prev $
                      filter (`elem` words "w r") approximants
    | isAffricate c = return prev
postNucleus prev@("l":_) = tails pLW (pure prev) (pure ("r":prev))
postNucleus prev = return prev

vowel :: [String] -> G [String]
vowel [] = error "can't start with vowel"
vowel (x:_) | isVowel x = error "can't double vowels"
vowel prev = do v <- sample vowels
                v' <- tails pYModified (pure v)
                            (heads pYIsPre (pure ("y"++v)) (pure (v++"y")))
                (:prev) <$> tails pPostR (pure v') (pure (v'++"r"))

postVowel :: [String] -> G [String]
postVowel x = return x          -- FIXME

syllables :: Int -> [String] -> G [String]
syllables 0 ctx = return (reverse ctx)
syllables n ctx = syllable ctx >>= syllables (n-1)

word :: Int -> G String
word n = concat <$> syllables n []


-- IO routines
dispWord :: Int -> IO ()
dispWord n = putStrLn =<< format disp <$> runIO (word n)

format :: [(String,String)] -> String -> String
format _ [] = []
format table s@(x:xs) = case lookupHead s table of
                          Nothing -> x : format table xs
                          Just (v,rest) -> v ++ format table rest
    where lookupHead _ [] = Nothing
          lookupHead x ((key,trans):rest)
              | startsWith key x = Just (trans, drop (length key) x)
              | otherwise = lookupHead x rest

disp = [("lh", "ɬ"),
        ("tsr", "ch"),
        ("sr", "sh"),
        ("θ", "th"),
        ("nk", "ng"),
        ("tθ", "tθ"),             -- to ensure we keep it
        ("iy", "í")] ++
       [(a++"y", a++"i") | a <- words "a e o u"]

ipa = [("lh", "ɬ"), ("sr", "ʃ"), ("nk", "ŋ"),
       ("a", "ɑ"), ("e", "ɛ"), ("i", "ɪ"), ("u", "ɯ")]
