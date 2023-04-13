--Ejercicio 1 (Generar una lista infinita de unos)
infUnos :: [Int]
infUnos = [1, 1..]

listaUnos :: [Int]
listaUnos = 1 : listaUnos

--Ejercicio 2 (Generar una lista infinita de naturales comenzando desde un n ́umero dado)
listaInfNat :: Int -> [Int]
listaInfNat n = [n, n+1..]


--Ejercicio 3 (Generar una lista con los primeros n naturales)
priNNat :: Int -> [Int]
priNNat n = [0..n]

--Ejercicio 4 (Retornar los primeros 5 elementos de una lista infinita de enteros positivos)
pri5Elem :: [Int] 
pri5Elem = take 5 (listaInfNat 0)

--Ejercicio 5 (Dada una lista, devuelve el cuadrado de todos sus elementos)
cuadList :: [Int] -> [Int]
cuadList = map cuad
    where cuad n = n*n

--Ejercicio 6 (Dado un entero positivo, retornar sus divisores)
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], mod n x == 0]

--Ejercicio 7 (Dada una lista de naturales, obtener la lista que contenga solo los n ́umeros primos de la lista original.)
soloPrimos :: [Int] -> [Int]
soloPrimos = filter esPrimo 
    where 
        esPrimo 1 = False
        esPrimo n = and [mod n i /= 0 | i<-[2..(n-1)]]

--Ejercicio 8 (Dada una lista de naturales, retornar la suma de los cuadrados de la lista)
sumCuadList :: [Int] -> Int
sumCuadList xs = sum (map cuad xs)
    where cuad n = n*n

--Ejercicio 9 (Dada una lista de naturales, retornar la lista con sus sucesores.)
succList :: [Int] -> [Int]
succList = map succ

--Ejercicio 10 (Dada una lista de enteros, sumar todos sus elementos.)
sumList :: [Int] -> Int
sumList xs = foldr (+) 0 xs

--Ejercicio 11 (Definir el factorial usando fold.)
fact :: [Int] -> Int
fact n = foldr (*) 1 n