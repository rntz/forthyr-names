{-# LANGUAGE GADTs #-}
module Gen3 where

import Prelude hiding (filter)

import Control.Applicative
import Control.Exception (assert)
import Control.Monad hiding (guard)
import Control.Monad.State.Strict (State, get, put, runState)
import Data.Maybe (mapMaybe)
import Data.Traversable
import System.Random (RandomGen, Random, StdGen)
import qualified System.Random as R

-- Utilities
isProb p = 0 <= p && p <= 1.0

replicateA i p = sequenceA (replicate i p)

endsWith :: String -> String -> Bool
endsWith ending string =
    ending == reverse (take (length ending) $ reverse string)

startsWith begin string = begin == take (length begin) string



-- Distributions, possibly empty
data P a where
    Pure :: a -> P a
    Ap :: P (a -> b) -> P a -> P b
    Join :: P (P a) -> P a
    Guard :: (a -> Bool) -> P a -> P a
    -- allowed to be empty, indicating failure
    Choice :: [(Double, P a)] -> P a

zero = choice []

guard :: (a -> Bool) -> P a -> P a
guard p (Pure x) | p x = Pure x
                 | otherwise = error "you're fucked"
guard p (Choice ks) = Choice [(w, guard p k) | (w,k) <- ks]
guard p (Guard q k) = guard (\x -> p x && q x) k
guard p k = Guard p k

choice = Choice

instance Show a => Show (P a) where
    show (Pure x) = "Pure " ++ show x
    show (Ap _ _) = "Ap _ _"
    show (Join x) = "Join (" ++ show x ++ ")"
    show (Guard _ x) = "Guard _ (" ++ show x ++ ")"
    show (Choice ps) = "Choice " ++ show ps

instance Functor P where
    fmap f (Pure x) = Pure (f x)
    fmap f (Ap (Pure g) a) = Ap (Pure (f . g)) a
    fmap f x = Ap (Pure f) x

instance Applicative P where
    pure = Pure
    Pure f <*> a = fmap f a
    f <*> Pure a = fmap ($ a) f
    f <*> a = Ap f a

instance Monad P where
    return = pure
    Pure x >>= f = f x
    k >>= f = Join (f <$> k)


-- Sampling
trySample :: RandomGen g => P a -> g -> (Maybe a, g)
trySample (Pure x) g = (Just x, g)
trySample (Ap f a) g =
    case trySample f g of
      (Nothing, g) -> (Nothing, g)
      (Just fv, g) -> (case trySample a g of
                         (Nothing, g) -> (Nothing, g)
                         (Just av, g) -> (Just (fv av), g))
trySample (Join k) g = case trySample k g of
                          (Nothing, g) -> (Nothing, g)
                          (Just k, g) -> trySample k g
trySample orig@(Guard p k) g =
    -- keep trying until it works
    case trySample k g of (Just x, g') | p x -> (Just x, g')
                          (_, g') -> trySample orig g'
trySample (Choice cs) g = findIt cs idx
    where
      total = sum $ map fst cs
      (idx,g') = R.randomR (0, total) g
      findIt ((w,c):cs) idx | idx <= w = trySample c g'
                            | otherwise = findIt cs (idx-w)
      findIt [] _ = (Nothing, g')

sample :: RandomGen g => P a -> g -> (a, g)
sample k g = let (Just x, g') = trySample (Guard (const True) k) g
             in (x, g')

trySampleIO d = R.getStdRandom (trySample d)
sampleIO d = R.getStdRandom (sample d)


-- Useful distributions
heads :: Double -> a -> a -> P a
heads p x y = assert (isProb p) $ choice [(p,pure x), (1-p,pure y)]
tails p x y = heads (1-p) x y
headsP p x y = join (heads p x y)
tailsP p x y = join (tails p x y)

uniform :: [a] -> P a
uniform l = choice [(1, pure x) | x <- l]
uniformP = join . uniform

weighted :: [(Double, a)] -> P a
weighted l = choice [(w, pure x) | (w,x) <- l]


-- Some stuff
vowel = weighted [(3,"a"), (1,"e"), (0.7,"i"), (1.4,"o"), (0.25, "u")]

consonant = choice [(2, stop),
                    (1, fricative),
                    (1, affricate),
                    (1, approximant)]

stop = weighted [(10, "t"), (6, "k"), (4, "p"), (5, "n")]
fricative = weighted [(1,"f"), (1,"s"), (1,"sr"), (1,"x"), (1,"θ"), (1,"lh")]
affricate = weighted [(1,"ts"), (1,"tsr"), (1,"tθ"), (1,"tlh"), (1,"kx")]
approximant = weighted [(1,"l"), (1,"w"), (1,"r")]

isStop x = x `elem` words "t k p n"
isFricative x = x `elem` words "f s sr x θ lh"
isAffricate x = x `elem` words "ts tsr tθ tlh kx"
isApproximant x = x `elem` words "l w r"
isCompound x = x `elem` words "sr lh ts tsr tθ tlh kx"

pVowelY = 0.12             -- probability of y-modified vowel
pPreY = 0.7                -- probability that y is a pre- not post-modification

-- Determining legitimacy of next characters
okay :: [String] -> Bool
okay (x:xs)
    | isVowel x = not (null xs || isVowel (head xs))
okay [c] = True
okay (c:prev:prevs) =
    not (needsVowel prev
         || isRepeat prev c     -- no repeats
         -- no (stops or fricatives) followed by stops
         || (isStop c && (isStop prev || isFricative prev))
         -- x cannot be followed by stops or fricatives
         || (endsWith "x" prev && (isStop c || isFricative c))
         -- compounds are single phonemes, no splitting them up
         || isCompound (prev ++ c)
         -- r needs to sit between a consonant and a vowel
         || (c == "r" && not (isVowel prev)))
okay [] = undefined             -- should never happen

isRepeat :: String -> String -> Bool
isRepeat a b = a == b || startsWith a b

needsVowel :: String -> Bool
needsVowel x = endsWith "w" x
               || endsWith "h" x
               || (endsWith "r" x && not (isVowel x))

-- UGH.
isVowel [x] = x `elem` "aeiou"
isVowel (x:"y") = x `elem` "aeiou"
isVowel (x:"yr") = x `elem` "aeiou"
isVowel ('r':[x]) = x `elem` "aeiou"
isVowel ('r':x:"y") = x `elem` "aeiou"
isVowel ('r':x:"yr") = x `elem` "aeiou"
isVowel ('r':x:"r") = x `elem` "aeiou"
isVowel ('y':[x]) = x `elem` "aeiou"
isVowel ('y':x:"y") = x `elem` "aeiou"
isVowel ('y':x:"yr") = x `elem` "aeiou"
isVowel ('y':x:"r") = x `elem` "aeiou"
isVowel ('r':'y':[x]) = x `elem` "aeiou"
isVowel ('r':'y':x:"y") = x `elem` "aeiou"
isVowel ('r':'y':x:"yr") = x `elem` "aeiou"
isVowel ('r':'y':x:"r") = x `elem` "aeiou"
isVowel _ = False


-- Basic generators
genConsonant (x:_) | needsVowel x = zero
genConsonant ctx = guard okay ((:ctx) <$> consonant)

genVowel [] = zero
genVowel (x:_) | isVowel x = zero
genVowel ctx = do v <- vowel
                  x <- tailsP pVowelY (pure v)
                              (heads pPreY ("y"++v) (v++"y"))
                  return (x:ctx)

-- sequenceA important here to avoid >>=, I think
genSyllable :: [String] -> P [String]
genSyllable ctx = do ctx_1 <- genPreVowel ctx
                     ctx_2 <- genVowel ctx_1
                     genPostVowel ctx_2

genPreVowel ctx = do ctx_1 <- genConsonant ctx
                     headsP 0.1 (genConsonant ctx_1) (return ctx_1)
genPostVowel ctx = return ctx      -- FIXME

genSyllables :: Int -> [String] -> P [String]
genSyllables 0 ctx = return ctx
genSyllables n ctx = genSyllable ctx >>= genSyllables (n-1)

word :: Int -> P String
word n = concat . reverse <$> genSyllables n []


-- IO routines
mkWord :: Int -> IO ()
mkWord n = putStrLn =<< sampleIO (word n)
