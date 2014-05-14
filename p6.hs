isPalindrome xs = case xs of
                    [] -> True
                    [x] -> True
                    otherwise -> (head xs == last xs) && (isPalindrome $ tail . init $ xs)
isPalindrome' xs = xs == (reverse xs)
