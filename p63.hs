import Data.List
import Data.Maybe

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

minHeight n = ceiling . logBase 2 . fromIntegral $ (n+1)

leftHeight n = minHeight n - 1

fullNodes h = 2^h - 1

leftNodes n = let
                  lh = leftHeight n
              in
                  if n >= 1 + fullNodes lh + fullNodes (lh-1) then
                        fullNodes lh
                  else
                        n - 1 - (fullNodes (lh - 1))

completeBinaryTree 0 = Empty
completeBinaryTree 1 = (Branch 'x' Empty Empty)
completeBinaryTree n = (Branch 'x' (completeBinaryTree ln) (completeBinaryTree rn))
                where ln = leftNodes n
                      rn = n - 1 - ln

isCompleteBinaryTree t = subt == completeBinaryTree cntt
                where (subt, cntt) = subCount t
                      subCount Empty = (Empty, 0)
                      subCount (Branch _ lt rt) = (Branch 'x' slt srt, clt + crt + 1)
                               where (slt, clt) = subCount lt
                                     (srt, crt) = subCount rt

