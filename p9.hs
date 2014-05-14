pack :: (Eq a) => [a] -> [[a]]
pack = foldr fun []
            where fun = (\x acc -> if (not . null $ acc) && (x == (head (head acc))) then
                                        (x:head acc) : (tail acc)
                                   else
                                        ([x]:acc))
