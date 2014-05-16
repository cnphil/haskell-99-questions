import Data.List
import Data.Maybe

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

minNodes' 0 = 0
minNodes' 1 = 1
minNodes' n = minimum [1 + minNodes' a + minNodes' b | a <- [n-2..n-1], b <- [n-2..n-1], a+b > 2*n-4]

maxHeight' 0 = 0
maxHeight' 1 = 1
maxHeight' n = maximum [1 + max lc rc | l <- [0..n-1], let lc = maxHeight' l, let rc = maxHeight' (n-1-l), lc-rc <= 1, rc-lc <= 1]

minNodesSeq = 0:1:zipWith ((+).(+1)) minNodesSeq (tail minNodesSeq)
minNodes = (minNodesSeq !!)

minHeight n = ceiling $ logBase 2 $ fromIntegral (n+1)
maxHeight n = (fromJust $ findIndex (>n) minNodesSeq) - 1

hbalTreeNodes x n = [t | h <- [minHeight n..maxHeight n], t <- filter ((==n).countNodes) (hbalTree x h)]
                    where
                        countNodes Empty = 0
                        countNodes (Branch _ lt rt) = 1 + countNodes lt + countNodes rt
