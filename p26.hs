import Control.Monad

combinations :: Int -> [a] -> [[a]]
combinations n xs
    | null xs || n == 0 = []
    | n == length xs = [xs]
    | otherwise = combinations n (tail xs)  ++ (map (head xs :) (combinations (n-1) (tail xs)))

