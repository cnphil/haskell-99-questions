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

layout' Empty _ w = (Empty, w)
layout' (Branch x lt rt) h w = (Branch (x, (lw+1, h)) lt' rt', rw)
                        where (lt', lw) = layout' lt (h+1) w
                              (rt', rw) = layout' rt (h+1) (lw+1)

layout t = fst $ layout' t 1 0
