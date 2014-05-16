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

tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )

depth :: Tree a -> Int
depth Empty = 0
depth (Branch a l r) = max (depth l) (depth r) + 1

layout t =
           let
              dep = depth t
              layout' Empty _ w = Empty
              layout' (Branch x lt rt) h w = Branch (x, (w, h)) lt' rt'
                                    where lt' = layout' lt (h+1) (w-(2^(dep-(h+1))))
                                          rt' = layout' rt (h+1) (w+(2^(dep-(h+1))))
           in
              layout' t 1 (2^(dep - 1) - 1)
