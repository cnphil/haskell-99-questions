data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

cbalTree 0 = [Empty]
cbalTree n = [Branch 'x' lc rc | i <- [q..q+r], lc <- cbalTree i, rc <- cbalTree (n-1-i)]
             where (q, r) = quotRem (n-1) 2

