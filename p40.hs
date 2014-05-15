sieve :: [Int] -> [Int]
sieve (p:ps) = p : (sieve $ filter ((/= 0).(`mod` p)) ps)

primes :: [Int]
primes = 2 : (sieve [3, 5..])

ceilSqrt = ceiling . sqrt . fromIntegral

isPrime n | n <= 1 = False
          | otherwise = null $ filter ((== 0).(n `mod`)) [2..(min (n-1) (ceilSqrt n))]

goldbach n = g primes
            where g (p:ps') = if isPrime (n-p) then
                                (p, n - p)
                              else
                                g ps'
