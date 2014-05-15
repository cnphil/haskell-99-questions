group :: [Int] -> [a] -> [[[a]]]
group [_] xs = [[xs]]
group (y:ys) xs = do
    (picked, rest) <- pick y xs
    map (picked:) (group ys rest)

pick y xs
    | y == 0 = [([], xs)]
    | null xs = []
    | otherwise = map (\(a,b) -> ((head xs):a,b)) (pick (y-1) (tail xs)) ++
                  (map (\(a,b) -> (a,(head xs):b)) (pick y (tail xs)))
    
