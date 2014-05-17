data Friendly a = Edge [(a, a)]
               deriving (Show, Eq)

paths :: Int -> Int -> Friendly Int -> [[Int]]
paths = paths' [] 

paths' ps s t (Edge es)
    | s == t = [[s]]
    | otherwise = (es >>= f)
        where f (a, b)
                = if a /= s || b `elem` ps then
                    []
                  else
                    map (a:) (paths' (a:ps) b t (Edge es))

cycle :: Int -> Friendly Int -> [[Int]]
cycle v (Edge es) = (es >>= g)
    where
        g (a, b) = if v /= a then
                      []
                   else
                      map (v:) $ paths b v (Edge es)
          
