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
toBinary 0 = [0]
toBinary 1 = [1]
toBinary n = reverse (mod n 2 : toBinary (div n 2))
