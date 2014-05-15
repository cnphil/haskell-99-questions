data MyList a = Single a | Multiple Int a
    deriving (Show)

encodeDirect :: (Eq a) => [a] -> [MyList a]
encodeDirect = foldr fun []
        where fun x acc = if (not . null $ acc) && (x == (whichElem $ head acc)) then
                            increase (head acc) : (tail acc)
                          else
                            (Single x) : acc
              whichElem (Single x) = x
              whichElem (Multiple _ x) = x
              increase (Single x) = (Multiple 2 x)
              increase (Multiple n x) = (Multiple (n+1) x)
