import Data.List
import Data.Maybe
import Control.Monad

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

ds2tree "" = (Empty, "")
ds2tree ('.':ps) = (Empty, ps)
ds2tree (p:ps) = (Branch p l r, rrest)
        where (l, lrest) = ds2tree ps
              (r, rrest) = ds2tree lrest

tree2ds Empty = "."
tree2ds (Branch p l r) = [p] ++ tree2ds l ++ tree2ds r

