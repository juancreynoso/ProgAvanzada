--Ejercicio 1 (Definir el tipo Nat)
data Nat = Zero | Suc Nat 

--instanciamos la clase Show en donde se muestran los numeros como sucesores de cero
instance Show Nat where
    show :: Nat -> String
    show Zero = "Cero"
    show (Suc n) = "Suc " ++ show n

--instanciamos la clase Show donde se muestran los numeros
--instance Show Nat where
--   show :: Nat -> String
--   show = show.natToInt

instance Eq Nat where
    n == m = natToInt n == natToInt m

instance Ord Nat where
    n >= m = natToInt n >= natToInt m
    n <= m = natToInt n <= natToInt m


--Ejercicio 2 (Definir la función natToInt : Nat → Int que dado un n ́umero Nat retorna
--su entero correspondiente.)
natToInt :: Nat -> Int 
natToInt Zero = 0
natToInt (Suc n) = 1 + natToInt n

--Ejercicio 3
intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Suc (intToNat (n-1))

--Ejercicio 4 (Definir la funcion sumaNat : Nat → Nat → Nat, la cual suma dos numeros Nat)
sumaNat :: Nat -> Nat -> Nat
sumaNat x y = intToNat (natToInt x + natToInt y)

--Ejercicio 5
data Tree a = Nil | Node (Tree a) a (Tree a) deriving Show

--Ejercicio 6
size:: Tree a -> Int
size Nil = 0
size (Node hi r hd) = 1 + size hi + size hd

--Ejercicio 7
height :: Tree a -> Int
height Nil = 0
height (Node hi r hd) = 1 + max (height hi) (height hd)
