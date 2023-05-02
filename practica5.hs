
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use splitAt" #-}
import GHC.Base (VecElem(Int16ElemRep))
import Language.Haskell.TH (PatSynArgs(InfixPatSyn))
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use and" #-}
{-# HLINT ignore "Use product" #-}
{-# HLINT ignore "Use sum" #-}

--Ejercicio 1 (Generar una lista infinita de unos)
infUnos :: [Int]
infUnos = [1, 1..]

listaUnos :: [Int]
listaUnos = 1 : listaUnos

--Ejercicio 2 (Generar una lista infinita de naturales comenzando desde un n ́umero dado)
listaInfNat :: Int -> [Int]
listaInfNat n = [n, n+1..]

--Ejercicio 3 (Generar una lista con los primeros n naturales)
priNumNat :: Int -> [Int]
priNumNat n = [1..n]

--Ejercicio 4 (Retornar los primeros 5 elementos de una lista infinita de enteros positivos)
pri5Elem :: [Int] 
pri5Elem = take 5 (listaInfNat 0)

--Ejercicio 5 (Dada una lista, devuelve el cuadrado de todos sus elementos)
cuadList :: [Int] -> [Int]
cuadList xs = map cuad xs
    where cuad n = n*n

--Ejercicio 6 (Dado un entero positivo, retornar sus divisores)
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], mod n x == 0]


--Ejercicio 7 (Dada una lista de naturales, obtener la lista que contenga solo los n ́umeros primos de la lista original.)
soloPrimos :: [Int] -> [Int]
soloPrimos n = filter esPrimo n
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
fact :: Int -> Int
fact n = foldr (*) 1 [1..n]

--Ejercicio 12 (Redefinir la función and tal que and xs se verifica si todos los elementos de xs son verdaderos.)
and' :: [Bool] -> Bool
and' xs = foldl (&&) True xs

--Ejercicio 13 (Usando foldl o foldr definir una función tam :: [a]->Int que devuelve la cantidad de elementos de una lista dada. 
--Dar un ejemplo en los cuales foldr y foldl evaluen diferente con los mismos parametros.)

tamr :: [a] -> Int
tamr [] = 0
tamr xs = foldr acc 0 xs
    where 
        acc :: a -> Int -> Int
        acc _ n = n + 1

taml :: [a] -> Int
taml [] = 0
taml xs = foldl acc 0 xs
    where
        acc :: Int -> a -> Int
        acc n _ = n + 1


--Ejercicio 14 (Dada una lista de enteros, retornar sus sucesores.)
listSucc :: [Int] -> [Int]
listSucc xs = [x + 1 | x <- xs]

--Ejercicio 15 (Dada una lista de enteros, retornar sus cuadrados.)
listCuad :: [Int] -> [Int]
listCuad xs = [x * x | x <- xs]

--Ejercicio 16 (Dada una lista de enteros, retornar los elementos pares que sean mayores a 10)
listMay10 :: [Int] -> [Int]
listMay10 xs = [x | x <- xs, x > 10 && even x]

--Ejercicio 17 (Dado un entero, retornar sus divisores.)
divDeNum :: Int -> [Int]
divDeNum n = [x | x <- [1..n], mod n x == 0]

--Ejercicio 18 (Definir la función todosOcurrenEn :: Eq a => [a] -> [a] -> Bool tal que todosOcurrenEn xs ys se verifica si todos los elementos de xs son elementos de ys.)
todosOcurrenEn :: Eq a => [a] -> [a] -> Bool
todosOcurrenEn xs ys = length [x | x <- xs, x `elem` ys] == length xs

--Ejercicio 19 (Dado un natural n, retornar los números primos comprendidos entre 2 y n.)
primosComp :: Int -> [Int]
primosComp n = [x | x <- [2..n], esPrimo x]
    where 
        esPrimo 1 = False
        esPrimo n = and [mod n i /= 0 | i<-[2..(n-1)]]

--Ejercicio 20 (Dadas dos listas de naturales, retornar su producto cartesiano.)
prodCart :: [Int] -> [Int] -> [(Int,Int)]
prodCart xs ys = [(x, y)| x <- xs, y <- ys]

--Ejercicio 21 (Dadas una lista y un elemento retornar el número de ocurrencias del elemento x en la lista ys.)
cantOcu :: Eq a => [a] -> a -> Int
cantOcu ys x = length [y | y <- ys, x == y]

--Ejercicio 22 (Escribir la funcion split2 :: [a] - > [([a],[a])], que dada una lista xs, devuelve la lista con todas las formas de partir xs en dos.)
split2 :: [a] -> [([a], [a])]
split2 xs = [(take n xs, drop n xs) | n <- [0..length xs]]

--Ejercicio 23 (Definir una función que, dada una lista de enteros, devuelva la suma de la suma de todos los segmentos iniciales.)
sumaSeg :: [Int] -> Int
sumaSeg xs = sum [sum (take i xs) | i <- [1.. length xs]]

--Ejercicio 24 (Definir la lista infinita de los numeros pares.)
infPares :: [Int]
infPares = [x | x <- [1..], even x]
