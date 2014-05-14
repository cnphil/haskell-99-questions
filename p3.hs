elementAt :: (Integral b) => [a] -> b -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)
