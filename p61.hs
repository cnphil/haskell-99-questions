import Data.List
import Data.Maybe

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

countLeaves Empty = 0
countLeaves (Branch a Empty Empty) = 1
countLeaves (Branch a lt rt) = countLeaves lt + countLeaves rt

collectLeaves Empty = []
collectLeaves me@(Branch a Empty Empty) = [a]
collectLeaves (Branch a lt rt) = collectLeaves lt ++ collectLeaves rt

