module Lib
    ( ProbabilityMonad
    , Dist
    , probability
    , percent
    , infer
    , mostLikely
    , fnub
    , merge
    , support
    , uniform
    , weighted
    , pthen
    , join2
    , pmap
    ) where

import ProbabilityMonad
import Data.Ratio
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)

-- | Query functions
probability :: (a -> Bool) -> Dist a -> Rational
probability pred (Dist as) = sum [ p | (p,a) <- as, pred a]

percent :: (a -> Bool) -> Dist a -> Double
percent p dist = (* 100) $ (fromIntegral n) / (fromIntegral d)
  where [n,d] = [numerator, denominator] <*> [probability p dist]

mostLikely :: Dist a -> a
mostLikely (Dist (a:as)) = go as a
  where go [] (_,a) = a
        go ((p,a):as) (p',a')
          | p > p'    = go as (p,a)
          | otherwise = go as (p',a')

infer :: (a -> b) -> (b -> Bool) -> Dist a -> Dist a
infer select pred as =
  let pGiven = probability (pred . select) as
  in  Dist [ (p / pGiven, a) | (p,a) <- unpackD as, pred $ select a ]

-- | Utility
-- | combines equivalent events and removes zero-probility events
fnub :: Ord a => Dist a -> Dist a
fnub = merge . support

support :: Dist a -> Dist a
support (Dist as) = Dist [ (p,a) | (p,a) <- as, p > 0 ]

merge :: Ord a => Dist a -> Dist a
merge (Dist as) = Dist $ fmap sumPs gs
  where as' = sortBy (comparing snd) as
        gs = groupBy (\a b -> snd a == snd b) as'
        sumPs xs = (sum $ map fst xs, snd $ head xs)

-- | Implementation
instance Functor Dist where
  fmap f (Dist as) = Dist $ [ (p, f a) | (p, a) <- as ]

instance Applicative Dist where
  pure = unitD
  (Dist fs) <*> (Dist as) = Dist [ (p'*p, f a) | (p, a) <- as, (p', f) <- fs ]
  (Dist as) *> (Dist bs) = Dist [ (p'*p, b) | (p, a) <- as, (p', b) <- bs ]

instance Monad Dist where
  return = unitD
  (>>=) = joinD
  (>>) = (*>)

instance ProbabilityMonad Dist where
  uniform = uniformD
  weighted = weightedD
  pthen as f = joinD as f'
    where f' a = fmap (\b -> (a,b)) (f a)
  join2 as bs f = joinD bs (f as)
  pmap f = fmap f

-- | Underlying implementation
newtype Dist a = Dist [(Rational, a)] deriving Show
unpackD :: Dist a -> [(Rational, a)]
unpackD (Dist xs) = xs

unitD :: a -> Dist a
unitD x = Dist [(1%1, x)]

uniformD :: [a] -> Dist a
uniformD xs = Dist $ uni xs 0
  where
    uni [] _ = []
    uni (x:xs) l = (1 % (toInteger $ 1 + l + length xs), x) : (uni xs (l + 1))

weightedD :: [(Rational, a)] -> Dist a
weightedD = Dist

joinD :: Dist a -> (a -> Dist b) -> Dist b
joinD (Dist as) f = Dist [ (p*p', b) | (p,a) <- as, (p',b) <- unpackD $ f a ]
