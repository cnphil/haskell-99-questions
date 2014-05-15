import Data.List

huffman :: [(Char, Int)] -> [(Char, [Char])]
huffman xs = myformat $ myhuff (length xs - 1) (shape xs)

shape xs = [([[]], [a], b) | (a, b) <- xs]

mysort xs = sortBy (\(_, _, c) (_, _, d) -> compare c d) xs

huffcombine (a, b, c) (d, e, f) = (map ('0':) a ++ map ('1':) d, b ++ e, c + f)

myhuff n cs
    | n == 0 = cs
    | otherwise = myhuff (n-1) (huffcombine c1 c2 : crst)
                  where (c1:c2:crst) = mysort cs

myformat ((code, name, _):xs) = sortBy (\(a,_) (b,_) -> compare a b) (zip name code)
