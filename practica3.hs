--Ejercicio 1
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists xs [] = xs
mergeLists [] ys = ys
mergeLists (x:xs) (y:ys) 
                  | x <= y = x : mergeLists xs (y:ys)
                  | otherwise = y : mergeLists ys (x:xs)

--Ejercicio 2

--Ejercicio 3
power2 :: Int -> Int
power2 0 = 1
power2 1 = 2
power2 n = 2 * power2 (n-1)

--Ejercicio 4
toBinary :: Int -> [Int]
toBinary 0 = []
toBinary 1 = [1]
toBinary n =  toBinary(div n 2) ++ [mod n 2]

--Ejercicio 5
binarioPar :: Int -> Bool
binarioPar 0 = True
binarioPar 1 = False
binarioPar n
            | last (toBinary n) == 1 = False
            | otherwise = True

--Ejercicio 6
distanciaH :: Eq a => [a] -> [a] -> Int
distanciaH [] [] = 0
distanciaH xs [] = 0
distanciaH [] ys = 0
distanciaH (x:xs) (y:ys) 
                        | x /= y = 1 + distanciaH xs ys
                        | otherwise = 0 + distanciaH xs ys

--Ejercicio 7
cuadPerf :: Int -> [Int]
cuadPerf n = [x | x <- [0..n], x*x == n]

--Ejercicio 8
repetidos ::  a -> Int -> [a]
repetidos x 0 = []
repetidos x 1 = [x]
repetidos x n = x : repetidos x (n-1)

--Ejercicio 10
posicionesC :: [Char] -> Char -> Int -> [Int]
posicionesC [] c pos = []
posicionesC (x:xs) c pos
                        | x /= c = posicionesC xs c (pos+1)
                        | otherwise = pos : posicionesC xs c (pos+1)
                    


