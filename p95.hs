import Data.List

fullWords :: Int -> String
fullWords n = concatMap g $ intersperse '-' (show n)
    where g '-' = "-"
          g '1' = "one"
          g '2' = "two"
          g '3' = "three"
          g '4' = "four"
          g '5' = "five"
          g '6' = "six"
          g '7' = "seven"
          g '8' = "eight"
          g '9' = "nine"
          g '0' = "zero"

