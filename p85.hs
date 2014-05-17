import Data.List

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

iso :: (Eq a) => Graph a -> Graph a -> Bool
iso (Graph vs1 es1) (Graph vs2 es2) = or $ map (\pm -> table (remap pm (Graph vs1 es1)) == table (Graph vs2 es2)) (permutations vs1)

remap pm (Graph vs1 es1) = Graph vs1 (es1 >>= f)
    where
        sub = zip vs1 pm
        subTo a = snd . head $ filter ((==a).fst) sub
        f (a, b) = [(subTo a, subTo b)]

table (Graph vs es) = map getConnected vs
    where getConnected v = map (isPair v) vs
          isPair a b  = (a,b) `elem` es || (b,a) `elem` es

graphG1 = Graph [1, 2, 3, 4, 5, 6, 7, 8]
		  [(1, 5), (1, 6), (1, 7), (2, 5), (2, 6), (2, 8),
		   (3, 5), (3, 7), (3, 8), (4, 6), (4, 7), (4, 8)]
 
graphH1 = Graph [1, 2, 3, 4, 5, 6, 7, 8]
		  [(1, 2), (1, 4), (1, 5), (6, 2), (6, 5), (6, 7),
		   (8, 4), (8, 5), (8, 7), (3, 2), (3, 4), (3, 7)]

-- iso graphG1 graphH1
-- should be True
