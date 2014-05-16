data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

cbalTree 0 = [Empty]
cbalTree n = [Branch 'x' lc rc | i <- [q..q+r], lc <- cbalTree i, rc <- cbalTree (n-1-i)]
             where (q, r) = quotRem (n-1) 2


rotate :: Tree a -> Tree a
rotate Empty = Empty
rotate (Branch a lc rc) = Branch a (rotate rc) (rotate lc)

symmetric :: (Eq a) => Tree a -> Bool
symmetric t = sub t == sub (rotate t)

sub Empty = Empty
sub (Branch a lc rc) = Branch 'x' (sub lc) (sub rc)

symCbalTrees = filter symmetric . cbalTree
