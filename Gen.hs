module Gen where

import Prelude hiding (filter)

import Debug.Trace (trace, traceStack)

import Control.Applicative
import Control.Exception (assert)
import Control.Monad
import Control.Monad.State.Strict (State, get, put, state, runState, evalState)

import Data.List (inits, tails)
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

always x = freqs [(1,x)]

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

runWith :: StdGen -> G a -> a
runWith g k = evalState k g

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

-- Bernoulli distribution (biased coin flip)
bern :: Double -> G a -> G a -> G a
bern 1 x _ = x
bern 0 _ y = y
bern p x y = assert (isProb p) $ choice [(p, x), (1-p, y)]

uniform :: [G a] -> G a
uniform l = choice [(1,x) | x <- l]


-- Some semantic tests
isStop x = x `elem` words "t k p n"
isFricative x = x `elem` words "f s sr x θ lh"
isAffricate x = x `elem` words "ts tsr tθ tlh kx"
isApproximant x = x `elem` words "l w r h hw"
isNasal = (== "n")
isCompound x = x `elem` words "sr lh ts tsr tθ tlh kx"

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

isRepeat :: String -> String -> Bool
isRepeat a b = last a == head b
-- isRepeat a b = a == b || startsWith a b

needsVowel :: String -> Bool
needsVowel x = endsWith "w" x
               || x == "h"
               || (endsWith "r" x && not (isVowel x))

data SyllablePos = C1 | C2 | V | C3 -- positions in CCVC structure

-- Determining legitimacy of next phonemes
ok :: SyllablePos -> [String] -> String -> Bool
ok pos prev c | not (okIn pos c && okAfter prev c) = False
ok C2 ctx@(c1:_) c2
    -- C1 x cannot have a C2
    | endsWith "x" c1 = -- nope "x_"
                        False
    -- C1 l must be followed by an approximant
    | startsWith "l" c1 && not (isApproximant c2) = -- nope "l[SFA]"
                                                    False
    | isFricative c1 && isStop c2 = -- nope "FS"
                                    False
    -- TODO: remove this rule?
    | isStop c1 && isNasal c2 = -- nope "SN"
                                False
  where nope msg = trace ("-- c2 " ++ msg ++ ": " ++ concat (reverse $ c2:ctx))
ok _ _ _ = True

okIn :: SyllablePos -> String -> Bool
-- TODO: should we allow affricates in C3? then can't say "cats"
okIn C3 c = not (needsVowel c) && not (isAffricate c)
--okIn C3 c = isStop c || c `elem` words "l x s θ"
okIn _ _ = True

okAfter :: [String] -> String -> Bool
okAfter l v | isVowel v = not (null l || isVowel (head l))
okAfter [] c = c /= "r"
okAfter ctx@(prev:_) c =
    let err s = trace ("-- " ++ s ++ ": " ++ concat (reverse $ c:ctx)) in -- TODO: remove
    not (needsVowel prev
         || isRepeat prev c     -- no repeats
         || (isAffricate prev && isAffricate c)
         -- no lh followed by l
         || (prev == "lh" && c == "l")
         -- x cannot be followed by "h"
         || (endsWith "x" prev && c == "h")
         -- compounds are single phonemes, no splitting them up
         || isCompound (prev ++ c)
         -- consonantal r needs to come after a consonant
         || (c == "r" && isVowel prev))


-- Probability parameters
pYModified = 0.12          -- probability of y-modified vowel
pYIsPre = 0.7              -- probability that y is a pre- not post-modification
pPostR = 0.15              -- probability of r-post-modified vowel

-- Syllable structure is C(C)V(C). We label these C1(C2)V(C3).
pC2 = 0.17                      -- probability of C2 occurring.
pC3 = 0.4                       -- probability of C3 occurring.

pTerminalC3 = 0.6               -- probability of ending a word w/ a consonant
pForceTerminalC3 = max 0 (pTerminalC3 - pC3)

-- Frequency distributions
vowels = freqs [(3,"a"), (1,"e"), (0.7,"i"), (1.4,"o"), (0.25, "u")]

stops = freqs [(10, "t"), (6, "k"), (2.5, "p"), (8, "n")]
fricatives = freqs [(1,"f"), (1,"s"), (0.5,"sr"),
                    (0.8,"x"), (0.8,"θ"), (1,"lh")]
affricates = freqs [(1,"ts"), (1,"tsr"), (0.75,"tθ"), (0.5,"tlh"), (0.5,"kx")]
approximants = freqs [(2,"l"), (0.8,"w"), (1,"r"), (1, "h"), (0.5, "hw")]

consonants = merge [(5, stops),
                    (2, fricatives),
                    (1, affricates),
                    (3, approximants)]

c1Consonants = filter (okIn C2) consonants
c2Consonants = filter (okIn C2) consonants
c3Consonants = filter (okIn C3) consonants


-- Basic generation
gen :: SyllablePos -> [String] -> Freq String -> G [String]
gen pos prev dist = (:prev) <$> sample (filter (ok pos prev) dist)

option :: Double -> SyllablePos -> [String] -> Freq String -> G [String]
option p pos prev dist = bern p (sample $ mapFreqs f dist) (pure prev)
    where f x | ok pos prev x = x:prev
              | otherwise = prev

syllable :: [String] -> G [String]
syllable prev = do prev <- consonant1 prev
                   prev <- consonant2 prev
                   prev <- vowel prev
                   consonant3 prev

consonant1 (x:_) | needsVowel x = error ("needs vowel: " ++ show x)
consonant1 prev = gen C1 prev c1Consonants
consonant2 prev = option pC2 C2 prev c2Consonants
consonant3 prev = option pC3 C3 prev c3Consonants

vowel :: [String] -> G [String]
vowel [] = error "can't start with vowel"
vowel (x:_) | isVowel x = error "can't double vowels"
vowel prev = do v <- sample vowels
                v' <- bern (1-pYModified) (pure v)
                           (bern pYIsPre (pure ("y"++v)) (pure (v++"y")))
                postR <- if null prev || notElem 'r' (head prev)
                         then bern pPostR (pure True) (pure False)
                         else return False
                pure ((if postR then (v'++"r") else v') : prev)

word :: G String
word = do len <- randomR (2,5)
          concat . reverse <$> syllables len []
    where
      syllables :: Int -> [String] -> G [String]
      syllables 0 accum@(x:_)
          | isVowel x = option pForceTerminalC3 C3 accum c3Consonants
      syllables 0 accum = return accum
      syllables n accum = syllable accum >>= syllables (n-1)


-- IO routines
showWords :: [(String,String)] -> Int -> IO ()
showWords table n = do words <- replicateM n (runIO word)
                       forM_ words $ putStrLn . format table
dispWords = showWords disp

showWordsWith table n g = mapM_ (putStrLn . format table)
                               (runWith (R.mkStdGen g) $ replicateM n word)
dispWordsWith = showWordsWith disp

format :: [(String,String)] -> String -> String
format _ [] = []
format table s@(x:xs) = case lookupHead s table of
                          Nothing -> x : format table xs
                          Just (v,rest) -> v ++ format table rest
    where lookupHead _ [] = Nothing
          lookupHead x ((key,trans):rest)
              | startsWith key x = Just (trans, drop (length key) x)
              | otherwise = lookupHead x rest

disp = [("lh", "ɬ"), ("tsr", "č"), ("sr", "sh"), ("ts", "ç"),
        ("hw","ƕ"),
        -- voicing rules
        --("nth", "nð"), ("nt", "nd"), ("lk", "lg"),
        -- ("θ", "th"),
        ("nk", "ng"),
        ("tθ", "tθ")]             -- to ensure we keep it
       ++ [(a++"y", a++"i") | a <- words "a e o u"]

ipa = [("lh", "ɬ"), ("sr", "ʃ"), ("nk", "ŋ"), ("hw", "ʍ"),
       ("a", "ɑ"), ("e", "ɛ"), ("i", "ɪ"), ("u", "ɯ")]
