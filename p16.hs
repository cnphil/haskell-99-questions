dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEvery' n xs
    where
        dropEvery' _ [] = []
        dropEvery' 1 (_:ps) = dropEvery' n ps
        dropEvery' n' (p:ps) = p:(dropEvery' (n'-1) ps)
