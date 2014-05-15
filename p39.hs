sieve :: [Int] -> [Int]
sieve (p:ps) = p : (sieve $ filter ((/= 0).(`mod` p)) ps)

primes :: [Int]
primes = 2 : (sieve [3, 5..])

primesR low high = takeWhile (<=high) $ dropWhile (<low) primes
