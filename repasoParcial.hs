{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use product" #-}
{-# HLINT ignore "Use and" #-}
{-# HLINT ignore "Eta reduce" #-}
import Data.IntMap.Merge.Lazy (merge)
import Data.Char (ord)
import Data.List ( delete )
import Text.XHtml (alt)
import Language.Haskell.TH (prim)
import Distribution.Simple.Utils (xargs)

-- Practica 2

hd :: [a] -> a
hd (x:xs) = x

tl :: [a] -> [a]
tl [] = []
tl (x:xs) = xs

lt :: [a] -> a
lt [x] = x
lt (x:xs) = hd (rev (x:xs))

lt' :: [a] -> a
lt' [x] = x
lt' (x:xs) = lt xs

rev :: [a] -> [a]
rev [] = []
rev [x] = [x]
rev (x:xs) = rev xs ++ [x]


initt :: [a] -> [a]
initt [] = []
initt [x] = []
initt (x:xs) = rev (drop 1 (rev (x:xs)))

init2 :: [a] -> [a]
init2 [x] = []
init2 (x:y:ys) = x : init2 (y:ys)

max3 :: Int -> Int -> Int -> Int
max3 x y z 
        | x >= y && y >= z = x
        | y >= z && z >= x = y
        | otherwise = z


concatenar :: [a] -> [a] -> [a]
concatenar [] xs = xs
concatenar xs [] = xs
concatenar (x:xs) ys = x : concatenar xs ys

concatFin :: [a] -> [a] -> [a]
concatFin [] xs = xs
concatFin xs [] = xs
concatFin xs (y:ys) = y:concatenar ys xs

tomarVino :: Int -> [a] -> [a]
tomarVino 0 xs = []
tomarVino n [x] = [x]
tomarVino n (x:xs) = x : tomarVino (n-1) xs

tirar :: Int -> [a] -> [a]
tirar 0 xs = xs
tirar n [x] = []
tirar n (x:xs) = tirar (n-1) xs


orEx :: Bool -> Bool -> Bool
orEx True False = True
orEx False True = True
orEx True True = False
orEx False False = True

orEx2 :: Bool -> Bool -> Bool
orEx2 x y 
        | x && y || not x && not y = False
        | otherwise = True

orEx3 :: Bool -> Bool -> Bool
orEx3 True p = not p
orEx3 False q = q


nPrimo :: Int -> Bool
nPrimo x = cantDiv x <= 2
        where cantDiv n = length [x | x <- [1.. n], mod n x == 0] 


primosMenQue :: Int -> [Int]
primosMenQue n = [x | x <- [1.. (n-1)], nPrimo x]


listIgu :: Eq a => [a] -> [a] -> Bool
listIgu [] [] = True
listIgu xs [] = False
listIgu [] xs = False
listIgu (x:xs) (y:ys) = x == y && listIgu xs ys

palindromo :: Eq a => [a] -> Bool
palindromo [] = True
palindromo xs = rev xs == xs

-- Practica 3

mergeLt :: [Int] -> [Int] -> [Int]
mergeLt [] [] = []
mergeLt xs [] = xs
mergeLt [] ys = ys
mergeLt (x:xs) (y:ys)
                | x <= y = x : mergeLt xs (y:ys)
                | otherwise = y :  mergeLt (x:xs) ys

ordLt :: [Int] -> [Int]
ordLt [] = []
ordLt xs = m : ordLt (delete m xs) -- Se invoca la accion con la lista sin el m
        where m = minimum xs


power22 :: Int -> Int
power22 0 = 1
power22 1 = 2
power22 n = 2 * power22 (n-1)


intToBin :: Int -> [Int]
intToBin 0 = [0]
intToBin 1 = [1]
intToBin n = intToBin (div n 2) ++ [mod n 2]

isBin :: [Int] -> Bool
isBin xs = hd (rev xs) == 0

distH :: [Char] -> [Char] -> Int
distH [] [] = 0
distH xs [] = 0
distH [] xs = 0
distH (x:xs) (y:ys)  
                | x == y = 0 + distH xs ys
                | otherwise = 1 + distH xs ys


cuadradoP :: Int -> [Int]
cuadradoP n = [x | x <- [1..n], x*x == n]

repetidos :: Char -> Int -> [Char]
repetidos c 0 = []
repetidos c n = c : repetidos c (n-1)

posC :: [Char] -> Char -> Int -> [Int]
posC [] c pos = []
posC (x:xs) c pos
        | x == c = pos : posC xs c (pos+1)
        | otherwise = posC xs c (pos+1)

compactar :: Eq a => [a] -> [a]
compactar [] = []
compactar [x] = [x]
compactar (x:y:xs)
                | x /= y = x : compactar (y:xs)
                | otherwise = compactar (x:xs)


-- Practica 5


lInf :: [Int]
lInf = 1:lInf

lInfN :: Int -> [Int]
lInfN n = n: lInfN (n+1)

primerosNat :: Int -> [Int]
primerosNat n = [1.. n]

primeros5 :: [Int]
primeros5 = take 5 [1, 2..]

listaCuad :: [Int] -> [Int]
listaCuad [] = []
listaCuad xs = map cuad xs
        where cuad x = x*x


listaDivisores :: Int -> [Int]
listaDivisores n = filter (divisor n) (primerosNat n)
        where divisor n x = mod n x == 0


factorial :: Int -> Int
factorial n = foldr (*) 1 [1..n]

andd :: [Bool] -> Bool
andd xs = foldr (&&) True xs

tamanioL :: [a] -> Int
tamanioL xs = foldl acc 0 xs
        where acc n _ = n + 1


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs 


listaPrimos :: [Int] -> [Int]
listaPrimos [] = []
listaPrimos xs = filter nPrimo xs

sumCuad :: [Int] -> Int
sumCuad [] = 0
sumCuad xs = sum (map cuad xs)
        where cuad x = x*x

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores xs = map (+1) xs

cuadrados :: [Int] -> [Int]
cuadrados xs = [x*x | x <- xs]

sumarNums :: [Int] -> Int
sumarNums xs = foldl (+) 0  xs

todosEstanEn :: Eq a => [a] -> [a] -> Bool
todosEstanEn xs ys = length [x | x <- xs, x `elem` ys] == length xs

cantOcu2 :: Eq a => a -> [a] -> Int
cantOcu2 c xs = length [x | x <- xs, x == c]

splitt :: [a] -> [([a],[a])]
splitt xs = [(take n xs, drop n xs) | n <- [0.. length xs]]

sumaSegmentos :: [Int] -> Int
sumaSegmentos xs = sum [sum (take n xs) | n <- [0.. length xs]]


factAc :: Integer -> Integer -> Integer
factAc ac 0 = ac
factAc ac n = (factAc $! (ac*n)) (n-1)

infPares :: [Int]
infPares = [x | x <- [0,1..], even x]

prodCart :: [a] -> [b] -> [(a,b)]
prodCart xs ys = [(x, y) | x <- xs, y <- ys]

paresMay10 :: [Int] -> [Int]    -- Dada una lista retorna los pares mayores que 10
paresMay10 xs = [x | x <- xs, x > 10 && even x]

-- Practica 6

-- definicion de naturales
data Nat = Zero | Succ Nat 

instance Show Nat where
    show :: Nat -> String
    show Zero = "Zero"
    show (Succ n) = "Succ " ++ show n

instance Eq Nat where
    (==) :: Nat -> Nat -> Bool
    n == m = natToInt2 n == natToInt2 m

instance Ord Nat where
    (>=) :: Nat -> Nat -> Bool
    n >= m = natToInt2 n >= natToInt2 m
    (<=) :: Nat -> Nat -> Bool
    n <= m = natToInt2 n <= natToInt2 m

intToNat2 :: Int -> Nat
intToNat2 0 = Zero
intToNat2 n = Succ (intToNat2 (n-1))

natToInt2 :: Nat -> Int
natToInt2 Zero = 0
natToInt2 (Succ n) = 1 + natToInt2 n

sumaNat2 :: Nat -> Nat -> Nat
sumaNat2 n m = intToNat2 (natToInt2 n + natToInt2 m)


data Arbol a = Nil | Node (Arbol a) a (Arbol a)
    deriving Show


esNil :: Arbol a -> Bool
esNil Nil = True
esNil (Node i r d) = False

sumHijosDer :: Arbol Int -> Int
sumHijosDer Nil = 0
sumHijosDer (Node i r d) = r + sumHijosDer d

-- nodoPar :: Arbol a -> [Bool]
-- nodoPar (Node i r d) = [even r] ++ [even (nodoPar i)] ++ [even (nodoPar d)]

concNodos :: Arbol a -> [a]
concNodos Nil = []
concNodos (Node i r d) = [r] ++ concNodos i ++ concNodos d

altura :: Arbol a -> Int
altura Nil = 0
altura (Node i r d) = 1 + max (altura i) (altura d)

cantNodos :: Arbol a -> Int
cantNodos Nil = 0
cantNodos (Node i r d) = 1 + cantNodos i + cantNodos d

cantHojas :: Arbol a -> Int
cantHojas Nil = 0
cantHojas (Node i r d) = 
    if esNil i && esNil d
        then 1
        else cantHojas i + cantHojas d

sumNodes :: Arbol Int -> Int
sumNodes Nil = 0
sumNodes (Node i r d) = r + sumNodes i + sumNodes d


-- Ejercicios de repaso
 
--dado un número n y una lista xs, retorne true si y solo si hay repetidos en los primeros n 
--elementos de la lista xs

primerosNRep :: Int -> [Int] -> Bool  
primerosNRep n [] = False
primerosNRep 0 (x:xs) = False
primerosNRep n (x:xs) = cantOcu2 x (take n (x:xs)) > 1 || primerosNRep (n-1) xs
 


-- Matriz infinita de forma diagonal

matrizInf :: [(Int, Int)]
matrizInf = [(x, n-x) | n <- [0..], x <- [0..n]]

-- Producto entre Naturales
prodNat :: Nat -> Nat -> Nat
prodNat Zero _ = Zero
prodNat _ Zero = Zero
prodNat n m = intToNat2 $ natToInt2 n * natToInt2 m

prodNat2 :: Nat -> Nat -> Nat
prodNat2 Zero _ = Zero
prodNat2 _ Zero = Zero
prodNat2 (Succ n) (Succ m) = sumaNat3 (Succ n) (prodNat (Succ n) m)

sumaNat3 :: Nat -> Nat -> Nat
sumaNat3 Zero x = x
sumaNat3 x Zero = x
sumaNat3 (Succ n) (Succ m) = Succ (Succ (sumaNat3 n m)) 

