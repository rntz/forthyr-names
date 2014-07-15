module Gen where

import Prelude hiding (join)

import Control.Applicative
import Control.Exception (assert)
import Control.Monad.State.Strict (State, get, put, runState)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import qualified System.Random as R
import System.Random (RandomGen, Random, StdGen)

-- Represents probability distributions over a finite number of outputs.
newtype P a = P (Map a Rational)
         -- | L [(a, Rational)]
    deriving (Show, Eq, Ord)

always :: a -> P a
always x = P (Map.singleton x 1.0)

toList :: P a -> [(a, Rational)]
toList (P x) = Map.toList x
-- toList (L x) = x

values :: P a -> [a]
values (P x) = Map.keys x

fromList :: (Ord a) => [(a, Rational)] -> P a
fromList l = assert isDist $ P $ Map.fromListWith (+) l
    where isDist = 1 == sum (map snd l)

weighted :: Ord a => [(a, Rational)] -> P a
weighted weights = fromList [(a,w/total) | (a,w) <- weights]
    where total = sum (map snd weights)

-- fromList :: [(a,Rational)] -> P a
-- fromList l = assert isDist (L l)
--     where isDist = 1.0 == sum (map snd l)

lift :: (Ord b) => (a -> b) -> P a -> P b
lift f a = fromList [(f x, w) | (x,w) <- toList a]

ap :: (Ord c) => (a -> b -> c) -> P a -> P b -> P c
ap f a b = fromList [(f x y, xw * yw) | (x,xw) <- toList a, (y,yw) <- toList b]

-- sequence :: Ord a => [P a] -> P [a]
-- sequence (x:xs) = ap (:) x (sequence xs)
-- sequence [] = always []

joint :: (Ord a, Ord b) => P a -> P b -> P (a,b)
joint x y = fromList [((xv,yv), xp*yp)
                      | (xv,xp) <- toList x, (yv,yp) <- toList y]

either :: (Ord a) => P a -> P a -> P a
either x y = fromList $ halve (toList x) ++ halve (toList y)
    where halve l = [(v,p/2) | (v,p) <- l]

join :: Ord a => P (P a) -> P a
join nested = fromList [(x, dp*xp) | (d,dp) <- toList nested,
                                     (x,xp) <- toList d]

merge :: Ord a => [(P a, Rational)] -> P a
merge = join . weighted

bernoulli :: Ord a => Rational -> P a -> P a -> P a
bernoulli p x y = assert legit $ merge [(x,p), (y,1-p)]
    where legit = 0 <= p && p <= 1

-- better leave something left over!
filter :: Ord a => (a -> Bool) -> P a -> P a
filter p d = weighted [(x,w) | (x,w) <- toList d, p x]

-- Uses a double in the range [0,1) to sample from a distribution.
sampleWith :: Double -> P a -> a
sampleWith p dist = findIt (toRational p) (toList dist)
    where findIt p ((x,w):xs)
              | p <= w = x
              | otherwise = findIt (p-w) xs
          findIt _ [] = error "invalid probability distribution"


-- Random sampler monad
type Sample a = Sampler StdGen a
type Sampler g a = State g a

runIO :: Sample a -> IO a
runIO k = R.getStdRandom (runState k)

random :: (RandomGen g, Random a) => Sampler g a
random = do gen <- get
            let (r, gen') = R.random gen
            put gen'
            return r

-- Sample from a distribution
sample :: RandomGen g => P a -> Sampler g a
sample dist = do p <- random
                 return $ sampleWith p dist


-- Some stuff
vowel :: P String
vowel = weighted [("a",3), ("e",1), ("i",0.7), ("o",1.4), ("u", 0.25)]

consonant :: P String
consonant = merge [(stop, 2),
                   (fricative,1),
                   (affricate, 1),
                   (approximant,1)]

stop :: P String
stop = weighted [("t", 10), ("k", 6), ("p", 4), ("n", 5)]

fricative :: P String
fricative = weighted [("f",1), ("s",1), ("sr",1), ("x",1), ("θ",1), ("lh",1)]

affricate :: P String
affricate = weighted [("ts",1), ("tsr",1), ("tθ",1), ("tlh",1), ("kx",1)
                     ,("klh",1)]

approximant :: P String
approximant = weighted [("l",1), ("w",1), ("r",1)]


-- Our representation using strict maps is not great for large domains. So we
-- use Sample instead.
syllable :: Sample String
syllable = concat <$> sequence [ sample consonant, sample secondConsonant
                               , sample vowel
                               , sample thirdConsonant
                               , sample fourthConsonant]
    where secondConsonant = bernoulli pSecondConsonant consonant (always "")
          thirdConsonant = bernoulli pThirdConsonant consonant (always "")
          fourthConsonant = bernoulli pFourthConsonant consonant (always "")
          pSecondConsonant = 0.12
          pThirdConsonant = 0.1
          pFourthConsonant = 0.1
