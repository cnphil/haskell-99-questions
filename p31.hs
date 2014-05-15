ceilSqrt = ceiling . sqrt . fromIntegral

isPrime n | n <= 1 = False
          | otherwise = null $ filter ((== 0).(n `mod`)) [2..(min (n-1) (ceilSqrt n))]
