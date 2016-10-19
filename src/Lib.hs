module Lib
    ( ProbabilityMonad
    , Dist (Dist)
    , probability
    , percent
    , toPercent
    , infer
    , mostLikely
    , fnub
    , dedup
    , dedupf
    , support
    , uniform
    , probs
    , weighted
    , pthen
    , join2
    , pmap
    , ExpectationMonad
    , expectation
    , SamplingMonad
    , sample
    ) where
import ProbabilityMonad
import Data.Ratio
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import System.Random (randomR)

-- | Query functions
probability :: (a -> Bool) -> Dist a -> Rational
probability pred (Dist as) = go as 0
  where go [] p = p
        go ((p',a):as) p
           | pred a    = go as (p+p')
           | otherwise = go as p

percent :: (a -> Bool) -> Dist a -> Double
percent p as = toPercent $ probability p as

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
toPercent :: Rational -> Double
toPercent = (* 100) . fromRational

fnub :: Ord a => Dist a -> Dist a
fnub = dedup . support

support :: Dist a -> Dist a
support (Dist as) = Dist [ (p,a) | (p,a) <- as, p > 0 ]

dedup :: Ord a => Dist a -> Dist a
dedup = dedupf id

dedupf :: Ord b => (a -> b) -> Dist a -> Dist a
dedupf f (Dist as) = Dist $ fmap sumPs gs
  where key = f . snd
        as' = sortBy (comparing key) as
        gs = groupBy (\a b -> key a == key b) as'
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
  probs = Dist
  weighted = weightedD
  pthen as f = joinD as f'
    where f' a = fmap (\b -> (a,b)) (f a)
  join2 as bs f = joinD bs (f as)
  pmap f = fmap f

instance ExpectationMonad Dist where
  expectation e (Dist as) = go as 0
    where go [] total = total
          go ((p,a):as) t = go as $ (fromRational p)*(e a) + t

instance SamplingMonad Dist where
  sample (Dist as) g = let (i, g') = randomR (0, length as - 1) g
                           (_, a) = as!!i
                       in (a,g')

-- | Underlying implementation
newtype Dist a = Dist [(Rational, a)] deriving (Show, Eq)
unpackD :: Dist a -> [(Rational, a)]
unpackD (Dist xs) = xs

unitD :: a -> Dist a
unitD x = Dist [(1%1, x)]

uniformD :: [a] -> Dist a
uniformD xs = Dist $ uni xs 0
  where
    uni [] _ = []
    uni (x:xs) l = (1 % (toInteger $ 1 + l + length xs), x) : (uni xs (l + 1))

weightedD :: [(Integer, a)] -> Dist a
weightedD as = Dist $ [ (w%total, a) | (w,a) <- as]
  where (ws, _) = unzip as
        total = sum ws

joinD :: Dist a -> (a -> Dist b) -> Dist b
joinD (Dist as) f = Dist [ (p*p', b) | (p,a) <- as, (p',b) <- unpackD $ f a ]
