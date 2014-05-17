import Data.List
import Data.Ord
import Control.Monad
import Data.Function (on)

data Graph a = Graph [a] [(a, a)]
               deriving (Show, Eq)
 
data Adjacency a = Adj [(a, [a])]
		   deriving (Show, Eq)
 
regular :: Int -> Int -> [Graph Int]
regular n k
    | odd (n * k) = []
    | otherwise = filterCanon . (filterDegrees k) $ trees
        where trees = [Graph [1..n] edges | edges <- chose (n*k `div` 2) $ possibleEdges n]

filterDegrees k = filter (checkDegrees k)

checkDegrees k (Graph vs es) = all (==k) (map countDegrees vs)
    where countDegrees v = length $ filter (\(x,y)->x==v||y==v) es

filterCanon = nubBy ((==) `on` (canon . graphToAdj)) 

possibleEdges n = [(x,y) | x <- [1..n], y <- [(x+1)..n]]

chose 0 _ = [[]]
chose n [] = []
chose n (p:ps) = chose n ps ++ map (p:) (chose (n-1) ps)

graphToAdj :: (Eq a) => Graph a -> Adjacency a
graphToAdj (Graph [] _)      = Adj []
graphToAdj (Graph (x:xs) ys) = Adj ((x, ys >>= f) : zs)
   where 
      f (a, b) 
         | a == x = [b]
         | b == x = [a]
         | otherwise = []
      Adj zs = graphToAdj (Graph xs ys)
 
adjToGraph :: (Eq a) => Adjacency a -> Graph a
adjToGraph (Adj [])          = Graph [] []
adjToGraph (Adj ((v, a):vs)) = Graph (v : xs) ((a >>= f) ++ ys)
   where
      f x = if (v, x) `elem` ys || (x, v) `elem` ys
            then []
	    else [(v, x)]
      Graph xs ys = adjToGraph (Adj vs)
 
canon :: (Eq a, Ord a) => Adjacency a -> String
canon (Adj a) = minimum $ map f $ perm n
   where
      n = length a
      v = map fst a
      perm n = foldr (\x xs -> [i : s | i <- [1..n], s <- xs, i `notElem` s]) [[]] [1..n]
      f p = let n = zip v p
	    in show [(snd x, 
		      sort id $ map (\x -> 
		         snd $ head $ snd $ break ((==) x . fst) n) $ snd $ find a x)
		    | x <- sort snd n]
      sort f n = foldr (\x xs -> let (lt, gt) = break ((<) (f x) . f) xs
				 in lt ++ [x] ++ gt) [] n
      find a x = let (xs, ys) = break ((==) (fst x) . fst) a in head ys

