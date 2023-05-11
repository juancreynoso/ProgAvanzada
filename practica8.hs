
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

-- Ejercicio 3 (Cuantificadores para todo y existe con determinadas posiciones de una lista)
paraTodo :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
paraTodo xs ys elemPosI = and [elemPosI i ys | i <- xs]
       
identidad :: Int -> [a] -> a -- Esta funcion devuelve el elemento de la posicion i de la lista
identidad i xs = xs !! i

isPos :: (Num a, Ord a) => Int -> [a] -> Bool
isPos i xs = xs !! i == 0

existe :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
existe xs ys p = or [p i ys | i <- xs]

--existe' :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
--existe' indices xs p = foldl (||) True ys
--    where ys = [p i indices | i <- xs]

isEven :: Integral a => Int -> [a] -> Bool -- Funcion esPar esta forma de cuantificadores
isEven i xs = even (xs !! i)

-- Ejercicio 4 (Cuantificadores de sumatoria, productoria y contatoria de determinadas posiciones de una lista)

productoria :: Num a => [Int] -> [a] -> (Int -> [a] -> a)-> a
productoria indices ys p = product [p i ys | i <- indices]

sumatoria :: Num a => [Int] -> [a] -> (Int -> [a] -> a)-> a
sumatoria indices ys p = sum [p i ys | i <- indices]

contatoria :: Integral a => [Int] -> [a] -> (Int -> [a] -> Bool)-> Int
contatoria indices ys p = length [i | i <- indices, p i ys]