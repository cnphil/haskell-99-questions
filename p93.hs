import Data.List
import Data.Ord
import Control.Monad

puzzle :: [Rational] -> [String]
puzzle ps = map snd $ mycons equals ps

mycons f ps = concatMap (\m -> f (take m ps) (drop m ps)) [0..(length ps)]

plain :: [Rational] -> [Rational] -> [(Rational, String)]
plain [x] [] = [(x, show x)]
plain _ _ = []

equals :: [Rational] -> [Rational] -> [(Rational, String)]
equals [] _ = []
equals _ [] = []
equals low high = do
    vL <- mycons plain low ++ mycons adds low ++ mycons subs low ++ mycons muls low ++ mycons divs low
    vH <- mycons plain high ++ mycons adds high ++ mycons subs high ++ mycons muls high ++ mycons divs high
    guard $ (fst vL) == (fst vH)
    return ((fst vL), (snd vL) ++ " = " ++ (snd vH))

adds :: [Rational] -> [Rational] -> [(Rational, String)]
adds [] _ = []
adds _ [] = []
adds low high = do
    vL <- mycons plain low ++ mycons adds low ++ mycons subs low ++ mycons muls low ++ mycons divs low
    vH <- mycons plain high ++ mycons adds high ++ mycons subs high ++ mycons muls high ++ mycons divs high
    return ((fst vL) + (fst vH), "(" ++ (snd vL) ++ ") + (" ++ (snd vH) ++ ")")

subs :: [Rational] -> [Rational] -> [(Rational, String)]
subs [] _ = []
subs _ [] = []
subs low high = do
    vL <- mycons plain low ++ mycons adds low ++ mycons subs low ++ mycons muls low ++ mycons divs low
    vH <- mycons plain high ++ mycons adds high ++ mycons subs high ++ mycons muls high ++ mycons divs high
    return ((fst vL) - (fst vH), "(" ++ (snd vL) ++ ") - (" ++ (snd vH) ++ ")")

muls :: [Rational] -> [Rational] -> [(Rational, String)]
muls [] _ = []
muls _ [] = []
muls low high = do
    vL <- mycons plain low ++ mycons adds low ++ mycons subs low ++ mycons muls low ++ mycons divs low
    vH <- mycons plain high ++ mycons adds high ++ mycons subs high ++ mycons muls high ++ mycons divs high
    return ((fst vL) * (fst vH), "(" ++ (snd vL) ++ ") * (" ++ (snd vH) ++ ")")

divs :: [Rational] -> [Rational] -> [(Rational, String)]
divs [] _ = []
divs _ [] = []
divs low high = do
    vL <- mycons plain low ++ mycons adds low ++ mycons subs low ++ mycons muls low ++ mycons divs low
    vH <- mycons plain high ++ mycons adds high ++ mycons subs high ++ mycons muls high ++ mycons divs high
    guard (fst vH /= 0)
    return ((fst vL) / (fst vH), "(" ++ (snd vL) ++ ") / (" ++ (snd vH) ++ ")")

