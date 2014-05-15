import Data.List

sieve :: [Int] -> [Int]
sieve (p:ps) = p : (sieve $ filter ((/= 0).(`mod` p)) ps)

primes :: [Int]
primes = 2 : (sieve [3, 5..])

primeFactors n
    | n < 2 = []
    | otherwise = primeFactors' n primes
        where
            primeFactors' 1 _ = []
            primeFactors' n ps@(p:ps') = if n `mod` p == 0 then
                                            p:(primeFactors' (n `div` p) ps)
                                         else
                                            primeFactors' n ps'

prime_factors_mult = map (\a -> (head a, length a)) . groupBy (==) . primeFactors
