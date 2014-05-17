import Control.Monad
import Data.List
import Data.Ord
import Data.Array

vonKoch es = do
    let range = sort . nub $ (concatMap (\(a,b)->[a,b]) es)
    perm <- permutations range
    let sub = zip range perm
    let mappedTo x = snd . head $ filter ((==x).fst) sub
    let dists = sort $ map (\(x,y) -> abs $ mappedTo x - mappedTo y) es
    guard $ and $ zipWith (/=) dists (tail dists)
    return perm

vonKoch' edges = do
    let n = length edges + 1
    nodes <- permutations [1..n]
    let nodeArray = listArray (1,n) nodes
    let dists = sort $ map (\(x,y) -> abs (nodeArray ! x - nodeArray ! y)) edges
    guard $ and $ zipWith (/=) dists (tail dists)
    return nodes
