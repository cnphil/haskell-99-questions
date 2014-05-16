import Data.List
import Data.Maybe

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )

type Pos = (Int, Int)

layout :: Tree a -> Tree (a, Pos)
layout t = t'
  where (l, t', r) = layoutAux x1 1 t
        x1 = maximum l + 1
 
        layoutAux :: Int -> Int -> Tree a -> ([Int], Tree (a, Pos), [Int])
        layoutAux x y Empty = ([], Empty, [])
        layoutAux x y (Branch a l r) = (ll', Branch (a, (x,y)) l' r', rr')
          where (ll, l', lr) = layoutAux (x-sep) (y+1) l
                (rl, r', rr) = layoutAux (x+sep) (y+1) r
                sep = maximum (0:zipWith (+) lr rl) `div` 2 + 1
                ll' = 0 : overlay (map (+sep) ll) (map (subtract sep) rl)
                rr' = 0 : overlay (map (+sep) rr) (map (subtract sep) lr)
 
-- overlay xs ys = xs padded out to at least the length of ys
-- using any extra elements of ys
overlay :: [a] -> [a] -> [a]
overlay [] ys = ys
overlay xs [] = xs
overlay (x:xs) (y:ys) = x : overlay xs ys

-- phil:
-- this is not original , but man this is fucking awesome
-- just check out the dependence structure of the `where` clause
-- lazy evaluation rocks!

