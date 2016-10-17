module Main where

import Lib
import Text.Printf
import Data.Ratio

data Coin = H | T deriving (Show,Eq,Ord)

fairCoin :: Dist Coin
fairCoin = uniform [H,T]

flips :: Int -> Dist [Coin]
flips n = sequence $ replicate n fairCoin

d6coins :: Dist [Coin]
d6coins = do n <- uniform [1..6]
             flips n

numHeads :: [Coin] -> Int
numHeads = length . filter (==H)

inferHeads :: Int -> (Int, Double)
inferHeads k = (l, pl)
  where prior = do n <- uniform [1..6]
                   coins <- flips n
                   return (n, coins)
        posterior = infer snd ((== k) . numHeads) prior
        posteriorN = fnub $ fmap fst posterior
        l = mostLikely posteriorN
        pl = percent (== l) posteriorN

-- | Tally Sheet Helpers
diceBowl :: [Integer] -> [Int] -> Dist Int
diceBowl counts dice = fnub $ weighted $ map f $ zip counts dice
  where total = sum counts
        f (c,a) = (c % total, a)
        f :: (Integer, Int) -> (Rational, Int)

draw2 :: Dist Int -> Dist [Int]
draw2 bowl = fnub $ do dice <- sequence [bowl, bowl]
                       let d1 = minimum dice
                       let d2 = maximum dice
                       return [d1,d2]

roll2sum :: Int -> Int -> Dist Int
roll2sum d1 d2 = fnub $ do roll1 <- uniform [1..d1]
                           roll2 <- uniform [1..d2]
                           return $ roll1 + roll2

data Tally = LeftT | MiddleT | RightT deriving (Show, Eq, Ord)

tally :: Dist Int -> (Int -> Tally) -> Dist Tally
tally sums f = fnub $ fmap f sums

tallyDist :: Dist Int -> Int -> Dist ([Int], (Int, Int, Int))
tallyDist bowl rolls = fnub $
  do [d1,d2] <- draw2 bowl
     tallies <- sequence $ replicate rolls $ tally (roll2sum d1 d2) rules
     return ([d1,d2], (left tallies, middle tallies, right tallies))
  where
    [left, middle, right] = map (\x -> length . filter (== x)) [LeftT, MiddleT, RightT]
    rules n
      | n < 16 = LeftT
      | n > 16 = RightT
      | otherwise = MiddleT
    rules :: Int -> Tally

main :: IO ()
main = do putStrLn $ printf "G. Throw d6; P(H>=3) is %.2f%%" (percent ((>= 3) . numHeads) d6coins)
          putStrLn $ printf "J. Throw d6; N > 4; P(H=3) is %.2f%%"
                     (let dist = do n <- uniform [5,6]
                                    coins <- flips n
                                    return $ numHeads coins
                      in percent (==3) dist)
          putStrLn $ printf "L. Throw d6; H=1; most likely N is %d (P=%.2f%%)" answerL answerLP
{-
          putStrLn $ printf "N. P(d12 = 2 AND RightMarks = 3) is $.10f%%"
                      (let tallysheet = tallyDist bowl 3
                           pred (ds@[d1,d2], (l,m,r)) = ds == [12,12] && r == 3
                       in  percent pred tallysheet)
-}
  where (answerL, answerLP) = inferHeads 1
        bowl = diceBowl [2,4,6] [6,12,20]
