-- not original
-- needs rework

import Data.Ord (comparing)
import Data.Function (on)
 
type Coord     = (Int,Int)
type Word      = String
data Site      = Site {siteCoords :: [Coord], siteLen :: Int} deriving (Show,Eq)
data Crossword = Crossword {cwWords :: [Word], cwSites :: [Site]}  deriving (Show,Eq)
 
equaling = ((==) `on`)
 
-- convert the text lines from the file to the "Site" datatype, 
--   which contain the adjacent coordinates of the site and its length
toSites :: [String] -> [Site]
toSites lines = find (index_it lines) ++ find (transpose . index_it $ lines)
    where find       = map makePos . concatMap extractor
          extractor  = filter ((>1) . length) . map (filter ((=='.').snd)) . groupBy (equaling snd)
          index_it   = zipWith (\row -> zip [(col,row) | col <- [1..]]) [1..]
          makePos xs = Site {siteCoords = map fst xs, siteLen = length xs}
 
-- test whether there exist no two different letters at the same coordinate
noCollision :: [(String, Site)] -> Bool
noCollision xs = all allEqual groupedByCoord
    where groupedByCoord = map (map snd) . groupBy (equaling fst) . sortBy (comparing fst) . concatMap together $ xs
          allEqual []     = True
          allEqual (x:xs) = all (x==) xs
 
-- merge a word and a site by assigning each letter to its respective coordinate
together :: (Word, Site) -> [(Coord, Char)]
together (w,s) = zip (siteCoords s) w
 
-- returns all solutions for the crossword as lists of occupied coordinates and their respective letters
solve :: Crossword -> [[(Coord, Char)]]
solve cw = map (concatMap together) solution
    where solution = solve' (cwWords cw) (cwSites cw)
 
solve' :: [Word] -> [Site] -> [[(Word, Site)]]
solve' _     []     = [[]]
solve' words (s:ss) = if null possWords
                        then error ("too few words of length " ++ show (siteLen s))
                        else do try <- possWords
                                let restWords = Data.List.delete try words
                                more <- solve' restWords ss
                                let attempt = (try,s):more
                                Control.Monad.guard $ noCollision attempt
                                return attempt
    where possWords = filter (\w -> siteLen s == length w) words                       
 
-- read the content of a file into the "Crossword" datatype
readCrossword :: String -> Crossword
readCrossword = (\(ws,ss) -> Crossword ws (toSites (drop 1 ss))) . break (""==) . lines
