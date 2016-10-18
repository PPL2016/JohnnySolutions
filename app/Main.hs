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

getXinY :: Dist a -> (a -> Bool) -> Int -> Int -> Dist Bool
getXinY as f x y = go x y
  where
    as' = fnub $ pmap f as
    go :: Int -> Int -> Dist Bool
    go 0 0       = return True
    go x y
     | x > y     = return False
     | x <= 0    = getXinY as (not . f) y y
     | otherwise = do hit <- as'
                      if hit then go (x-1) (y-1)
                             else go x (y-1)

main :: IO ()
main = do putStrLn $ printf "G. Throw d6; P(H>=3) is %.2f%%" (percent ((>= 3) . numHeads) d6coins)
          putStrLn $ printf "J. Throw d6; N > 4; P(H=3) is %.2f%%"
                     (let dist = do n <- uniform [5,6]
                                    coins <- flips n
                                    return $ numHeads coins
                      in percent (==3) dist)
          putStrLn $ printf "L. Throw d6; H=1; most likely N is %d (P=%.2f%%)" answerL answerLP
          putStrLn $ printf "N. P(d12 = 2 AND RightMarks = 3) is %.5f%%"
                     (let pOf2d12 = probability (== [12,12]) $ draw2 bowl
                          tallyD = tally (roll2sum 12 12) rules
                          pOf3Right = probability (==True) $ getXinY tallyD (==RightT) 3 30
                      in  toPercent $ pOf2d12 * pOf3Right)
          putStrLn $ printf "O. P(all marks in left) == %.4f%%"
                      (let dist = do [d1,d2] <- draw2 bowl
                                     let sumD = roll2sum d1 d2
                                     let tallyD = tally sumD rules
                                     getXinY tallyD (==LeftT) 30 30
                       in percent (==True) dist)
  where (answerL, answerLP) = inferHeads 1
        bowl = diceBowl [9,9,14,14] [6,8,12,20]
