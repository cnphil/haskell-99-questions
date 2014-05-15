not' True = False
not' False = True

and' True True = True
and' _ _ = False

or' a b = not' (and' (not' a) (not' b))

nor' a b = not' $ or' a b

nand' a b = not' $ and' a b

xor' a b = (not' $ and' a b) `and'` (not' (and' (not' a) (not' b)))

equ' a b = (and' a b) `or'` (and' (not' a) (not' b))

impl' a b = (not' a) `or'` b

table :: (Bool -> Bool -> Bool) -> IO ()
table f = sequence_ g
    where g :: [IO ()]
          g = do
            b1 <- [True, False]
            b2 <- [True, False]
            return $ putStrLn $ show b1 ++ " " ++ show b2 ++ " " ++ show (f b1 b2)
