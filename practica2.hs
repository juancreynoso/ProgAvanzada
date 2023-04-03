{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Redundant bracket" #-}
import Distribution.Utils.Structured (containerStructure)
import Data.Bits (Bits(xor))
import Data.Time.Format.ISO8601 (yearFormat)

--Ejercicio 1
splitNum :: Int -> [Int]
splitNum n | n < 10 = [n] --caso base
           | otherwise = splitNum(div n 10) ++ [mod n 10]

--Ejercicio 2

head' :: [a] -> a
head' (x:xs) = x

tail' :: [a] -> [a]
tail' (x:xs) = xs

last' :: [a] -> a
last' x = head' (reverse x)

init' :: [a] -> [a]
init' x = reverse (tail' (reverse x))


--[1,2,3]
--x = 1
--xs = [2,3]

--Ejercicio 3

maxTres :: (Ord a) => a -> a -> a -> a
maxTres x y z
    | x >= y && x >= z = x
    | y >= x && y >= z = y
    | otherwise = z

--Ejercicio 4
conc :: [a] -> [a] -> [a]
conc xs [] = xs
conc [] ys = ys
conc (x:xs) ys = x:conc xs ys

long :: [a] -> Int
long [] = 0
long (x:xs) = 1 + long xs

tomar :: Int -> [a] -> [a]
tomar 0 x = []
tomar n [] = [] 
tomar n (x:xs) = x:tomar (n-1) xs

tirar :: Int -> [a] -> [a]
tirar n [] = []
tirar 0 x = x
tirar n (x:xs) = tirar (n-1) xs 

concFinal :: a -> [a] -> [a] 
concFinal x [] = [x]
concFinal x (y:ys) = y : concFinal x ys


--Ejercicio 5
abs' :: Int -> Int 
abs' x 
    | x >= 0 = x
    | otherwise = -x

--Ejercicio 6
edad :: (Int, Int, Int) -> (Int, Int, Int) -> Int
edad (d1, m1, a1) (d2, m2, a2)
    |((d1 <= d2) && (m1 <= m2)) = a2 - a1
    |(m1 < m2) = a2 - a1
    |otherwise = a2 - a1 - 1

--Ejercicio 7
xor' :: Bool -> Bool -> Bool
xor' True True = False
xor' True False = True
xor' False True = True
xor' False False = False

xor2 :: Bool -> Bool -> Bool
xor2 x y
    | x && y || not(x) && not(y) = False
    | otherwise = True


--Ejercicio 8
esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = and [(mod n i) /= 0 | i<-[2..(n-1)]]

--Ejercicio 9
listaPrimos :: Int -> [Int]
listaPrimos 0 = []
listaPrimos 1 = []
listaPrimos (n) = [n | n <- [1..n-1], esPrimo(n)] 

--Ejercicio 10
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

--Ejercicio 11
iguales :: (Eq a) => [a] -> [a] -> Bool
iguales [] [] = True
iguales [] [x] = False
iguales [x] [] = False
iguales (x:xs) (y:ys) = x == y && (iguales xs ys) 

--Ejercicio 12
esPalindromo :: (Eq a) => [a]  -> Bool
esPalindromo [] = True
esPalindromo [x] = True
esPalindromo (x:xs) = x:xs == reversa (x:xs)

