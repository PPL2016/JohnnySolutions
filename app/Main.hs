module Main where

import Lib
import Text.Printf
import Data.Ratio
import Data.List (nub)
import Math.Combinatorics.Exact.Binomial

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
diceBowl counts dice = fnub $ weighted $ zip counts dice

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
rules :: Int -> Tally
rules n
  | n < 16 = LeftT
  | n > 16 = RightT
  | otherwise = MiddleT

tally :: Dist Int -> (Int -> Tally) -> Dist Tally
tally sums f = fnub $ fmap f sums

binomial :: Integer -> Rational -> Dist Integer
binomial n p = Dist $ zip (go n) $ reverse [0..n]
  where np = 1 - p
        go :: Integer -> [Rational]
        go 0 = [np^n]
        go k = (((choose n k)%1) * (p^k) * (np^(n-k))) : (go (k-1))

main :: IO ()
main = do putStrLn $ printf "G. Throw d6; P(H>=3) is %.2f%%" (percent ((>= 3) . numHeads) d6coins)
          putStrLn $ printf "J. Throw d6; N > 4; P(H=3) is %.2f%%"
                     (let dist = do n <- uniform [5,6]
                                    coins <- flips n
                                    return $ numHeads coins
                      in percent (==3) dist)
          putStrLn $ printf "L. Throw d6; H=1; most likely N is %d (P=%.2f%%)" answerL answerLP
          putStrLn $ printf "N. P(d12 = 2 AND RightMarks = 3) is %.2f%%"
                      (let pOfd12s = probability (==[12,12]) $ draw2 bowl
                           tallyD = tally (roll2sum 12 12) rules
                           pOfRight = probability (==RightT) tallyD
                           pOf3Right = probability (==3) $ binomial 30 pOfRight
                       in  toPercent $ pOfd12s * pOf3Right)
          putStrLn $ printf "O. P(all marks in left) == %.2f%%"
                      (let allLeft = do [d1,d2] <- draw2 bowl
                                        let tallyD = tally (roll2sum d1 d2) rules
                                        let pOfLeft = probability (==LeftT) tallyD
                                        b <- binomial 30 pOfLeft
                                        return $ b == 30
                       in  percent id allLeft)
          putStrLn $ printf "P. expected marks in right == %.1f"
                      (let anyRight = do [d1,d2] <- draw2 bowl
                                         let tallyD = tally (roll2sum d1 d2) rules
                                         let pOfRight = probability (==RightT) tallyD
                                         binomial 30 pOfRight
                       in expectation fromInteger anyRight)
  where (answerL, answerLP) = inferHeads 1
        dice = [6,8,12,20]
        counts = [9,9,14,14]
        bowl = diceBowl counts dice
