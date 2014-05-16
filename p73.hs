import Control.Monad
import Data.Maybe

data Tree a = Node a [Tree a]
    deriving (Eq, Show)

tree1 = Node 'a' []
 
tree2 = Node 'a' [Node 'b' []]
 
tree3 = Node 'a' [Node 'b' [Node 'c' []]]
 
tree4 = Node 'b' [Node 'd' [], Node 'e' []]
 
tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

ipl' (Node _ ts) dep = dep + foldr (\t acc-> ipl' t (dep+1) + acc) 0 ts
ipl t = ipl' t 0

bottom_up (Node p ts) = concatMap bottom_up ts ++ [p]

display_lisp (Node p []) = [p]
display_lisp (Node p ts) = "(" ++ [p] ++ foldl (\a b -> a ++ " " ++ b) "" (map display_lisp ts) ++ ")"
