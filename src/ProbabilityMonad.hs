module ProbabilityMonad
  ( ProbabilityMonad
  , uniform
  , weighted
  , pthen
  , join2
  , pmap
  ) where
import Data.Ratio

-- | Exported interface
class Monad p => ProbabilityMonad p where
  uniform  :: [a] -> p a
  weighted :: [(Rational, a)] -> p a
  pthen    :: p a -> (a -> p b) -> p (a,b)
  join2    :: p a -> p b -> (p a -> b -> p c) -> p c
  pmap     :: (a -> b) -> (p a -> p b)
