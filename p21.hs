insertAt x xs n
    | n == 1 = x:xs
    | otherwise = (head xs):(insertAt x (tail xs) (n-1))
