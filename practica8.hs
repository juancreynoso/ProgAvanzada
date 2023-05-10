
-- Ejercicio 1
nand :: Bool -> Bool -> Bool
nand True True = False
nand True False = True
nand False p = True


-- Ejercicio 2
maj :: Bool -> Bool -> Bool -> Bool
maj True True _ = True
maj _ True True = True
maj True _ True = True
maj False False _ = False
maj False _ False = False
maj _ False False = False

-- Ejercicio 3
paraTodo :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
paraTodo xs ys p = and [p i ys | i <- xs]
       

--p :: (a -> Bool) -> Int -> [a] -> Bool
--p f i (x:xs)
--        | i == 0 = f x
--        | otherwise = p f (i-1) xs 


existe :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
existe xs ys p = or [p i ys | i <- xs]

isEven :: Int -> [Int] -> Bool
isEven i xs = mod (xs !! i) 2 == 0

productoria :: Num a => [Int] -> [a] -> (Int -> [a] -> a)-> a
productoria xs ys p = product [p i ys | i <- xs]
    where p i xs = xs !! i