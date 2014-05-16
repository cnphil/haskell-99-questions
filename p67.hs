import Data.List
import Data.Maybe
import Control.Monad

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

treeToString Empty = ""
treeToString (Branch x Empty Empty) = [x]
treeToString (Branch x l r) = [x] ++ "(" ++ treeToString l ++ "," ++ treeToString r ++ ")"

stringToTree :: (Monad m) => String -> m (Tree Char)
stringToTree str = liftM fst (mst str)
  where
    mst "" = return (Empty, "")
    mst [x] = return (Branch x Empty Empty, "")
    mst (x:y:ys)
        | x == ')' || x == ',' = return (Empty, y:ys)
        | x == '(' = fail "parse error"
        -- now x must be some value
        | y == '(' = do
                      (lt, lrest) <- mst ys
                      (rt, rrest) <- mst lrest
                      return (Branch x lt rt, rrest)
        | y == ')' || y == ',' = return (Branch x Empty Empty, ys)
        | otherwise = fail "parse error"

