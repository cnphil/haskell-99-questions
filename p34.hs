coprime x y = (gcd x y) == 1

totient n = length $ filter (coprime n) [1..n]
