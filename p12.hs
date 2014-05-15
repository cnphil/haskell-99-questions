pack :: (Eq a) => [a] -> [[a]]
pack = foldr fun []
            where fun = (\x acc -> if (not . null $ acc) && (x == (head (head acc))) then
                                        (x:head acc) : (tail acc)
                                   else
                                        ([x]:acc))

encode :: (Integral a) => (Eq b) => [b] -> [(a, b)]
encode = map (\ys -> (fromIntegral . length $ ys, head ys)) . pack

data MyList a = Single a | Multiple Int a
    deriving (Show)

encodeModified :: (Eq a) => [a] -> [MyList a]
encodeModified = map toMyList . encode
    where toMyList (1, x) = Single x
          toMyList (n, x) = Multiple n x

decodeModified :: [MyList a] -> [a]
decodeModified = concatMap myReplicate
    where myReplicate (Multiple n x) = replicate n x
          myReplicate (Single x) = [x]
