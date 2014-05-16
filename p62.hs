import Data.List
import Data.Maybe

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

internals :: Tree a => [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch a lt rt) = [a] ++ internals lt ++ internals rt

atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch x _ _) 1 = [x]
atLevel (Branch _ lt rt) n = atLevel lt (n-1) ++ atLevel rt (n-1)

