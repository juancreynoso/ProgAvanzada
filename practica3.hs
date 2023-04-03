--Ejercicio 1
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists xs [] = xs
mergeLists [] ys = ys
mergeLists (x:xs) (y:ys) 
                  | x <= y = x : mergeLists xs (y:ys)
                  | otherwise = y : mergeLists ys (x:xs)

--Ejercicio 2
ordList :: Ord a => [a] ->[a]
ordList [] = []
ordList (x:y:ys) 
                | x <= y = x : ordList (y:ys)
                | otherwise = y : ordList (x:ys)