--seperate the file into three files (factoring stuff, auxilery stuff, and general list/data manipulation things)

module PollardRHO (factorize, uniqueFactorize, fermatTest, totient, toFactorPairs, fromFactorPairs, uniquize) where

import Data.List
import Data.Ratio

import Atkin
import Diophantine

--data Factorization = Pairs [(Integral, Int)] | Factors [Integral]

--this function is terrible at factoring
psrand :: Integral a => a -> a -> a
psrand x n = (x ^ 2 + 1) `rem` n

pollardRHOi :: Integral a => a -> a -> a -> a -> a
pollardRHOi d x y n
    | d == n    = 1
    | d == 1    = pollardRHOi (gcd n (abs (x' - y'))) x' y' n
    | otherwise = d
    where x' = psrand x n
          y' = psrand (psrand y n) n

pollardRHO :: Integral a => a -> a
pollardRHO n = pollardRHOi 1 2 2 n

pollardRHOAlli :: Integral a => a -> [a] -> [a]
pollardRHOAlli n factors
    | n' == 1   = factors ++ [n]
    | otherwise = factors ++ [n'] ++ (pollardRHOAlli (n `quot` n') factors)
    where n' = pollardRHO n

pollardRHOAll :: Integral a => a -> [a]
pollardRHOAll n = pollardRHOAlli n []

trialDivideAll :: Integral a => a -> [a]
trialDivideAll n = sort $ trialDivideAlli n []

trialDivideAlli :: Integral a => a -> [a] -> [a]
trialDivideAlli n factors
    | n' == 1   = factors ++ [n]
    | otherwise = factors ++ [n'] ++ (trialDivideAlli (n `quot` n') factors)
    where n' = trialDivide n

trialDivide :: Integral a => a -> a
trialDivide n = trialDividei n 2 ((floor . sqrt . fromIntegral) n)

trialDividei :: Integral a => a -> a -> a -> a
trialDividei n cur top
    | cur > top         = n
    | n `rem` cur == 0  = cur
    | otherwise         = trialDividei n (cur + 1) top

--the limit for the bound may need to be changed
catchSmalls :: Integral a => [a] -> [a]
catchSmalls xs = (concat $ foldr ((:) . (\x -> trialDivideAll x)) [] $ filter (< mySmalls) xs) ++ (filter (>= mySmalls) xs)
    where mySmalls = 10000

--the top level function that invokes various methods to factor with
factorize :: Integral a => a -> [a]
factorize n = sort $ filter (/= 1) $ catchSmalls $ pollardRHOAll n

jPerms :: Integral a => (a, a) -> [(a,a)]
jPerms (x ,y)
    | y < 0     = []
    | otherwise = [(x, y)] ++ jPerms (x, y - 1)

divisors :: Integral a => a -> [a]
divisors n = (init . sort) (map fromFactorPairs (sequence (map jPerms (intoFactorPairs n))))

divisorsFactorPairs :: Integral a => [(a, a)] -> [a]
divisorsFactorPairs xs = (init . sort) (map fromFactorPairs (sequence (map jPerms xs)))
    
uniquize :: Eq a => [a] -> [a]
uniquize = ((map head) . group)

maxRepeat :: Eq a => [a] -> Int
maxRepeat = maximum . ((map length) . group)

uniqueFactorize :: Integral a => a -> [a]
uniqueFactorize = (uniquize . factorize)

squareFree :: Integral a => a -> Bool
squareFree n = 1 == (maxRepeat $ factorize n)

squareFreeFactorPairs :: Integral a => [(a, a)] -> Bool
squareFreeFactorPairs xs = 1 == (maximum $ map snd xs)

--there is a way to make fermatsTest pure (code a PRNG into the function and feed it a seed)
--this test misses carmicheal numbers(it will by definition)
fermatTest :: Integral a => a -> Bool
fermatTest n = ((a ^ (n - 1) `mod` n) == 1)
    where a = 2

fermatTesti :: Integral a => a -> a -> Bool
fermatTesti n x = ((x ^ (n - 1) `mod` n) == 1)
    
totient :: Integral a => a -> a
totient n = numerator ((n % 1) * (product (map (\x -> (1 - 1 % x)) d)))
    where d = uniqueFactorize n
    
intoFactorPairs :: Integral a => a -> [(a, a)]
intoFactorPairs n = map (\x -> (head x, fromIntegral (length x))) (group (factorize n))

toFactorPairs :: Integral a => [a] -> [(a, a)]
toFactorPairs xs = map (\x -> (head x, fromIntegral (length x))) (group xs)

fromFactorPairs :: Integral a => [(a, a)] -> a
fromFactorPairs xs = foldl' (\z (x, y) -> x ^ y * z) 1 xs

mobius :: Integral a => a -> a
mobius n
    | n == 1                                            = 1
    | (squareFreeFactorPairs fp) && even (length fp)    = 1
    | (squareFreeFactorPairs fp) && odd (length fp)     = -1
    | otherwise                                         = 0
    where fp = intoFactorPairs n

--this function can BE WAY MORE EFFECIENT if i can generate primes(what do you know, I can now (TODO))
--mertens :: Integral a => a -> a
mertens k = sum (map mobius [1..k])

order :: Integral a => a -> a -> a
order r n = head $ filter (\x -> (r ^ x `mod` n) == 1) [1..n]


--problem for Gosler
niceFactors :: Integral a => a -> [a]
niceFactors x = [1] ++ (uniquize $ sort $ map (foldl' (*) 1) $ uniquize $ sort $ filter (not . null) $ concat $ map tails $ permutations $ factorize x)

parify :: Integral a => [a] -> (a, a)
parify xs = (head xs, last xs)

nicePairs :: Integral a => [a] -> [(a, a)]
nicePairs xs
    | null xs           = []
    | (length xs) == 1  = [parify xs]
    | otherwise         = [parify xs] ++ (nicePairs ((tail . init) xs))
    
isValidPair :: Integral a => (a, a) -> Bool
isValidPair (x, y) = (x < 100) && (y < 100)

allValidPairs :: Integral a => a -> [(a, a)]
allValidPairs x = filter isValidPair $ nicePairs $ niceFactors x

doesOneKnow :: Integral a => a -> Bool
doesOneKnow x = (length (allValidPairs x)) == 1

possiblePairs :: Integral a => a -> [(a, a)]
possiblePairs x = [ (y, x - y) | y <- [1..(floor ((fromIntegral x) / 2))]]

couldOneKnow :: Integral a => a -> Bool
couldOneKnow x = True `elem` (map (\(n, m) -> doesOneKnow (n * m)) (possiblePairs x))

doesWork :: Integral a => (a, a) -> Bool
doesWork (n, m) = (not $ doesOneKnow (n * m)) && (not $ couldOneKnow (n + m)) && (1 == (length $ filter (\(n', m') -> couldOneKnow (n' + m')) $ allValidPairs (n * m)))

