import Data.List
import Data.Ord

knightsTo :: Int -> (Int, Int) -> [[(Int, Int)]]
knightsTo n p = [pos:path | (pos, path) <- tour (n*n)]
    where tour 1 = [(p, [])]
          tour m = [(pos', pos:path) |
                        (pos, path) <- tour (m-1),
                        pos' <- mySortBy (availables (pos:path)) (filter (`notElem` path) (move n pos))]
          availables path pos = length $ filter (`notElem` path) (move n pos)

mySortBy f xs = map snd (sortBy (comparing snd) [(f x, x) | x <- xs])

onBoard n (x,y) = x <= n && x >= 1 && y <= n && y >= 1

move n (x,y) = filter (onBoard n) (map pairPlus [(sx*dx,sy*dy) | dx <- [1,2], let dy = 3-dx, sx <- [-1,1], sy <- [-1,1]])
    where pairPlus (x1,y1) = (x1+x,y1+y)

