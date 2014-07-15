{-# LANGUAGE GADTs #-}
module Gen2 where

import Prelude hiding (filter)

import Control.Applicative
import Control.Exception (assert)
import Control.Monad
import Control.Monad.State.Strict (State, get, put, runState)
import Data.Maybe (mapMaybe)
import Data.Traversable
import System.Random (RandomGen, Random, StdGen)
import qualified System.Random as R

-- Utilities
isProb p = 0 <= p && p <= 1.0

replicateA i p = sequenceA (replicate i p)


-- Probability distributions
data D a where
    Always :: a -> D a
    -- list of (weight, branch)
    -- invariant: non-zero total weight
    Choice :: [(Double, D a)] -> D a
    Ap :: D (a -> b) -> D a -> D b

instance Show a => Show (D a) where
    show (Always x) = "Always " ++ show x
    show (Choice ps) = "Choice " ++ show ps
    show (Ap _ a) = "Ap _ _"

foldD :: (a -> b) -> ([(Double,b)] -> b) -> D a -> b
foldD i f (Always x) = i x
foldD i f (Choice ds) = f [(w, foldD i f x) | (w,x) <- ds]
foldD i f (Ap gd ad) = foldD i f (pushAp gd ad)

flattenD :: D a -> [(Double, a)]
flattenD (Always x) = [(1.0, x)]
flattenD (Choice ds) = [(dw*xw,x) | (dw,d) <- ds, (xw,x) <- flattenD d]
flattenD (Ap f a) = flattenD (pushAp f a)

pushAp :: D (a -> b) -> D a -> D b
pushAp fd ad = foldD (\f -> f <$> ad) Choice fd

sampleWith :: RandomGen g => D a -> g -> (a, g)
sampleWith (Always x) g = (x, g)
sampleWith (Choice cs) g = findIt cs idx
    where
      total = sum $ map fst cs
      (idx,g') = R.randomR (0, total) g
      findIt ((w,c):cs) idx | idx <= w = sampleWith c g'
                            | otherwise = findIt cs (idx-w)
      findIt [] _ = error "invalid probability distribution"
sampleWith (Ap f a) g =
    let (fv, g') = sampleWith f g
        (av, g'') = sampleWith a g'
    in (fv av, g'')

instance Functor D where
    fmap f (Always x) = Always (f x)
    fmap f (Choice xs) = Choice [(w, fmap f x) | (w,x) <- xs]
    fmap f (Ap g a) = Ap (fmap (f .) g) a

instance Applicative D where
    pure = Always
    Always f <*> a = fmap f a
    f <*> Always a = fmap ($ a) f
    f <*> a = Ap f a

instance Monad D where
    return = Always
    Always k >>= f = f k
    Choice ds >>= f = Choice [(w, d >>= f) | (w,d) <- ds]
    Ap fd ad >>= g = pushAp fd ad >>= g


-- Possibly-empty probability distributions
newtype P a = P { unP :: Maybe (D a) }
    deriving Show

-- smart constructors
zero = P Nothing

always :: a -> P a
always x = P (Just (Always x))

choice :: [(Double, P a)] -> P a
choice ps = P $ case ds of [] -> Nothing
                           [(_,x)] -> Just x
                           _ -> Just (Choice ds)
    where ds = [(w,x) | (w, P (Just x)) <- ps, w > 0]

filter :: (a -> Bool) -> P a -> P a
filter p k = k >>= (\x -> if p x then pure x else zero)

flatten :: P a -> [(Double, a)]
flatten (P Nothing) = []
flatten (P (Just d)) = flattenD d

sampleIO :: P a -> IO (Maybe a)
sampleIO (P Nothing) = return Nothing
sampleIO (P (Just x)) = Just <$> R.getStdRandom (sampleWith x)

instance Functor P where
    fmap f (P Nothing) = P Nothing
    fmap f (P (Just d)) = P (Just (fmap f d))

instance Applicative P where
    pure = P . Just . Always
    P Nothing <*> _ = P Nothing
    _ <*> P Nothing = P Nothing
    P (Just f) <*> P (Just a) = P (Just (f <*> a))

instance Monad P where
    return = pure
    P Nothing >>= f = P Nothing
    -- avoid using this if at all possible!
    P (Just a) >>= f = foldD (\x -> f x) choice a


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

-- sequenceA important here to avoid >>=, I think
syllable :: P String
syllable = concat <$> sequenceA [ consonant
                                , on 0.1 consonant
                                , vowel
                                , on 0.1 consonant]
    where on p x = headsP p x (pure "")
