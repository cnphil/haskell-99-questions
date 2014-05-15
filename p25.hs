import Control.Monad
import System.Random
import Data.List

rnd_permu :: [a] -> IO [a]
rnd_permu xs
    | null xs = return []
    | otherwise = do
        gen <- getStdGen
        let len = length xs
        let permu = (take len) . nub $ (randomRs (0, len-1) gen)
        return (foldr (\n acc -> ((xs!!n):acc)) [] permu)
