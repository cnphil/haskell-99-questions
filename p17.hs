split :: [a] -> Int -> ([a], [a])
split xs 0 = ([], xs)
split (x:xs) n = (x:front, rear)
    where (front, rear) = split xs (n-1)
