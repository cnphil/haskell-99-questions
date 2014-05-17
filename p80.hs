data Graph a = Graph [a] [(a, a)]
               deriving (Show, Eq)

data Adjacency a = Adj [(a, [a])]
               deriving (Show, Eq)

data Friendly a = Edge [(a, a)]
               deriving (Show, Eq)

graphToAdj :: (Eq a) => Graph a -> Adjacency a
graphToAdj (Graph [] _) = Adj []
graphToAdj (Graph (x:xs) es) = Adj ((x, es >>= getAdj) : rest)
    where getAdj e
            | fst e == x = [snd e]
            | snd e == x = [fst e]
            | otherwise = []
          Adj rest = graphToAdj (Graph xs es)

adjToGraph :: (Eq a) => Adjacency a -> Graph a
adjToGraph (Adj []) = Graph [] []
adjToGraph (Adj ((v,ps):xs)) = Graph (v:restv) ((ps >>= getAdj) ++ rest)
    where
        getAdj p
            | (p, v) `elem` rest || (v, p) `elem` rest = []
            | otherwise = [(v, p)]
        Graph restv rest = adjToGraph (Adj xs)
