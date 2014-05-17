import Data.List
import Data.Ord
import Control.Monad
import Data.Char

identifier :: String -> Bool
identifier "" = False
identifier ps@(p:_) = isLetter p && all (\c -> isLetter c || isDigit c || c == '-') ps && not (or (zipWith (\a b -> a == '-' && b == '-') ('x':ps++"-") (ps++"-")))
