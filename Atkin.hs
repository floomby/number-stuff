module Atkin (primes) where

import Data.List
import Data.List.Ordered
import Data.Maybe

polyOne :: Integral a => (a, a) -> a
polyOne (x, y) = 4 * x ^ 2 + y ^ 2

checkOne :: Integral a => (a, a) -> a -> Maybe a
checkOne (x, y) limit
    | (n <= limit) && ((n `rem` 12) `elem` [1,5])   = Just n
    | otherwise                                     = Nothing
    where n = polyOne (x, y)

polyTwo :: Integral a => (a, a) -> a
polyTwo (x, y) = 3 * x ^ 2 + y ^ 2

checkTwo :: Integral a => (a, a) -> a -> Maybe a
checkTwo (x, y) limit
    | (n <= limit) && (n `rem` 12 == 7)             = Just n
    | otherwise                                     = Nothing
    where n = polyTwo (x, y)

polyThr :: Integral a => (a, a) -> a
polyThr (x, y) = 3 * x ^ 2 - y ^ 2

checkThr :: Integral a => (a, a) -> a -> Maybe a
checkThr (x, y) limit
    | (n <= limit) && (x > y) && (n `rem` 12 == 11) = Just n
    | otherwise                                     = Nothing
    where n = polyThr (x, y)

checkPolys :: Integral a => (a, a) -> a -> [a]
checkPolys (x, y) limit = catMaybes [checkOne (x, y) limit, checkTwo (x, y) limit, checkThr (x, y) limit]

primesi :: Integral a => a -> [a]
primesi n = concat $ filter (odd . length) $ group $ sort $ tmp n >>= \(x, y) -> checkPolys (x, y) n

primes :: Integral a => a -> [a]
primes n = [2, 3] ++ (xs `minus` exps)
    where top = (floor . sqrt . fromIntegral) n
          xs = primesi n
          lit = filter (<= top) xs
          exps = sort $ lit >>= expToLim n

expToLim :: Integral a => a -> a -> [a]
expToLim lim num = takeWhile (<= lim) [ x * num ^ 2 | x <- [1..]]

tmp :: Integral a => a -> [(a, a)]
tmp n = [ (x, y) | x <- [1..((floor . sqrt . fromIntegral) n)], y <- [1..((floor . sqrt . fromIntegral) n)]]