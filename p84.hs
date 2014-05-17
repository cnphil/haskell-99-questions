import Data.Bool
import Data.List
import Data.Ord

data Graph a = Graph [a] [(a, a)]
               deriving (Show, Eq)

data Friendly a = Edge [(a, a)]
               deriving (Show, Eq)

paths :: Int -> Int -> Graph Int -> [[Int]]
paths = paths' [] 

paths' ps s t (Graph vv es)
    | s == t = [[s]]
    | otherwise = (es >>= f')
        where
            f (a, b)
                = if a /= s || b `elem` ps then
                    []
                  else
                    map (a:) (paths' (a:ps) b t (Graph vv es))
            f' (a, b) = f (a, b) ++ f (b, a)  

cycle :: Int -> Graph Int -> [[Int]]
cycle v (Graph vv es) = (es >>= g)
    where
        g (a, b) = if v /= a && v /= b then
                      []
                   else
                      map (v:) $ paths (a+b-v) v (Graph vv es)

spantree :: Graph Int -> [Graph Int]
spantree (Graph vs es) = (filter connected) . (filter edgeCount) $ allgraphs
    where
        n = length vs
        allgraphs = [Graph (pointsOf nes) nes | nes <- (foldr f [[]] es)]
        f e acc = acc ++ (map (e:) acc)
        pointsOf es = nub $ concatMap (\(a,b)->[a,b]) es
        edgeCount (Graph vs' es') = (length vs' == n) && (length es' == (n-1))
        connected (Graph vs' es') = all (/=[]) $ map (\v -> paths (head vs') v (Graph vs' es')) (tail vs') 

data GraphW a = GraphW [a] [(a, a, Int)]
                deriving (Show, Eq)

prim (GraphW vs es) = prim' [head vs] [] (length vs - 1) (GraphW vs es)

prim' chosenV chosenE 0 _ = GraphW chosenV chosenE
prim' chosenV chosenE remainE (GraphW vs es)
    = prim' (newV:chosenV) (newE:chosenE) (remainE - 1) (GraphW vs es)
        where
            availableE = filter (\(a,b,_)-> (a `elem` chosenV) `xor` (b `elem` chosenV)) es 
            newE@(a',b',c') = minimumBy (comparing (\(_,_,c)->c)) availableE
            newV = if a' `elem` chosenV then b' else a'

xor :: Bool -> Bool -> Bool
xor = (/=)

