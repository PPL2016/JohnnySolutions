module ProbabilityMonad
  ( ProbabilityMonad
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
import Data.Ratio (Rational)
import System.Random (RandomGen, StdGen)

-- | Exported interface
class Monad p => ProbabilityMonad p where
  uniform  :: [a] -> p a
  probs    :: [(Rational, a)] -> p a
  weighted :: [(Integer, a)] -> p a
  pthen    :: p a -> (a -> p b) -> p (a,b)
  join2    :: p a -> p b -> (p a -> b -> p c) -> p c
  pmap     :: (a -> b) -> (p a -> p b)

class ProbabilityMonad p => ExpectationMonad p where
  expectation :: (a -> Double) -> p a -> Double

class ProbabilityMonad p => SamplingMonad p where
  sample :: RandomGen g => p a -> g -> (a,g)
  stdSample :: p a -> StdGen -> a
