import System.Random
import Control.Monad
removeAt n xs = (xs!!(n-1), liftM2 (++) (take (n-1)) (drop n) xs)

rnd_select :: RandomGen g => [a] -> Int -> g -> ([a], g)
rnd_select _ 0 gen = ([], gen)
rnd_select [] _ gen = ([], gen)
rnd_select l cnt gen
    | cnt == (length l) = (l, gen)
    | otherwise         = rnd_select (snd $ removeAt (k+1) l) cnt gen'
                          where (k, gen') =
                                    randomR (0, (length l) - 1) gen

rnd_selectIO :: [a] -> Int -> IO [a]
rnd_selectIO l cnt = getStdRandom $ rnd_select l cnt
