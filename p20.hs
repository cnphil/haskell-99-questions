import Control.Monad
removeAt n xs = (xs!!(n-1), liftM2 (++) (take (n-1)) (drop n) xs)
