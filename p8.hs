compress :: (Eq a) => [a] -> [a]
compress = foldr fun []
            where fun = (\x acc -> if (not . null $ acc) && (x == head acc) then
                                        acc
                                   else
                                        (x:acc))
