pack :: (Eq a) => [a] -> [[a]]
pack = foldr fun []
            where fun = (\x acc -> if (not . null $ acc) && (x == (head (head acc))) then
                                        (x:head acc) : (tail acc)
                                   else
                                        ([x]:acc))

encode :: (Integral a) => (Eq b) => [b] -> [(a, b)]
encode = (map (\ys -> (fromIntegral . length $ ys, head ys))) . pack
