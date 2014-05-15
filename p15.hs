repli xs n = foldr (\x acc -> wrap n x acc) [] xs
        where wrap 0 x acc = acc
              wrap n x acc = x:(wrap (n-1) x acc)
