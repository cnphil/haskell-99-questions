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

dfs g i = reverse $ dfs' [i] [i] g i

dfs' stack visited (Graph vs es) i
   =
     if null unvisited then
        if 1 == length stack then
            visited
        else
            dfs' (tail stack) visited (Graph vs es) (head (tail stack))
     else
        dfs' (head unvisited : stack) (head unvisited : visited) (Graph vs es) (head unvisited)
     where
        unvisited = map (\(a,b)-> if a == i then b else a) $ filter (\(a,b)-> g (a,b) || g (b,a)) es
        g (a, b) = a == i && b `notElem` visited 

connectedComponents (Graph vs es) = foldr f [] vs
    where
        f v acc = if v `elem` (concat acc) then
                     acc
                  else
                     dfs (Graph vs es) v : acc
