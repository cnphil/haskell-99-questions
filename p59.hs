data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

rotate :: Tree a -> Tree a
rotate Empty = Empty
rotate (Branch a lc rc) = Branch a (rotate rc) (rotate lc)

symmetric :: (Eq a) => Tree a -> Bool
symmetric t = sub t == sub (rotate t)

sub Empty = Empty
sub (Branch a lc rc) = Branch 'x' (sub lc) (sub rc)

construct :: (Ord a) => [a] -> Tree a
construct [] = Empty
construct (x:xs) = Branch x (construct ls) (construct rs)
                   where ls = filter (<=x) xs
                         rs = filter (>x) xs

hbalTree _ 0 = [Empty]
hbalTree x 1 = [Branch x Empty Empty]
hbalTree x n = [Branch x lc rc | lc <- hbalTree x (n-1), rc <- hbalTree x (n-1)]
            ++ [Branch x lc rc | lc <- hbalTree x (n-1), rc <- hbalTree x (n-2)]
            ++ [Branch x lc rc | lc <- hbalTree x (n-2), rc <- hbalTree x (n-1)]

