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

bfs (Graph vs es) [] coloring = Just coloring
bfs (Graph vs es) (p:ps) coloring = if length adjColor > 1 then Nothing else nextStep
    where
        adjacent = delete p $ nub $ concatMap (\(a,b)->[a,b]) $ filter (\(a,b)-> a==p || b==p) es
        colored i = not . null $ filter ((==i).fst) coloring
        colorOf i = snd . head $ filter ((==i).fst) coloring
        mycolor = colorOf p
        adjColor = nub $ map (\i -> if colored i then colorOf i else 1-mycolor) adjacent
        nextStep = bfs (Graph vs es) (ps++newps) (coloring++newcolor)
        newps = filter (\i -> i `notElem` ps && (not . colored $ i)) adjacent
        newcolor = map (\i -> (i, 1-mycolor)) $ filter (not.colored) adjacent

bipart (Graph vs es) = all (/=Nothing) $ map (\i->bfs (Graph vs es) [i] [(i,0)]) vs 
