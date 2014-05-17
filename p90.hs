queens :: Int -> [[Int]]
queens = queens' []

queens' prev n
    | length prev == n = [prev]
    | otherwise = concatMap g [1..n]
        where g x = let posx = length prev in
                    if x `notElem` prev && and (map (\(a,b) -> (abs $ a-x) /= (abs $ b-posx)) (zip prev [0..])) then
                        queens' (prev++[x]) n
                    else
                        []
