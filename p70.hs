import Control.Monad
import Data.Maybe

data Tree a = Node a [Tree a]
    deriving (Eq, Show)

nnodes :: Tree a -> Int
nnodes (Node _ ts) = 1 + foldr (\t acc -> acc + (nnodes t)) 0 ts

treeToString :: Tree Char -> String
treeToString (Node c ts) = [c] ++ concatMap treeToString ts ++ "^"

stringToTree :: String -> Tree Char
stringToTree str = fromJust . fst $ mst str
mst ('^':xs) = (Nothing, xs)
mst (x:xs) = (Just (Node x (fst subs)), (snd subs))
    where subs = mysub xs
          mysub ys = 
            let
                (jValue, jRest) = mst ys
            in
                if jValue == Nothing then
                    ([], jRest)
                else
                    let mValue = mysub jRest in
                        ((fromJust jValue):(fst mValue), (snd mValue))

