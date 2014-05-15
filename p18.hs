slice :: [a] -> Int -> Int -> [a]
slice xs n m
    | n > m = []
    | n == 1 = (head xs) : slice (tail xs) 1 (m-1)
    | otherwise = slice (tail xs) (n-1) (m-1)

slice' xs i k | i>0 = take (k-i+1) $ drop (i-1) xs
