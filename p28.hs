import Data.Function
import Data.List
import Control.Monad
lsort :: [[a]] -> [[a]]
lsort = foldr fun []
        where 
           fun n ns
            | null ns = [n]
            | length n <= (length . head $ ns) = n : ns
            | otherwise = head ns : (fun n (tail ns))

lfsort = concat . lsort . groupBy ((==) `on` length) . lsort
