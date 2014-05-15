rotate xs n
    | n == 0 = xs
    | null xs = []
    | n < 0 = rotate xs (n+(length xs))
    | otherwise = rotate ((tail xs) ++ [head xs]) (n-1)
