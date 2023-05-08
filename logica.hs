nand:: Bool -> Bool->Bool
nand True True = False
nand True False = True
nand False p = True
-- ---------------------------------------------------- 
-- maj retorna True sii al menos 2 argumentos son True.
-- ----------------------------------------------------
maj :: Bool -> Bool -> Bool -> Bool
maj True True _ = True
maj _ True True = True
maj True _ True = True
maj False False _ = False
maj False _ False = False
maj _ False False = False

-- ---------------------------------------------------- 
-- Para las siguientes funciones se debe respetar el 
-- perfil propuesto.
-- La lista [Int] de paraTodo representa las posiciones 
-- sobre las que cuantificamos en [a].
-- Mientras que (Int -> [a] -> Bool) es la propiedad.
--		Ejemplo: paraTodo [0,1,2,3] [4,1,2,6] even 
--		retorna False, ya que existe una posición 
--		en la que el elemento de la lista es impar. 
--		paraTodo [0,2,4,6] [2,2,4,4,4,5,6] even  
--		retorna True.
-- ----------------------------------------------------
paraTodo :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
paraTodo xs ys p = and [p i ys | i <- xs]
       

p :: (a -> Bool) -> Int -> [a] -> Bool
p f i (x:xs)
        | i == 0 = f x
        | otherwise = p f (i-1) xs


        
-- ----------------------------------------------------
-- La lista [Int] de paraTodo representa las posiciones 
-- sobre las que cuantificamos en [a]. 
-- (Int -> [a] -> Bool) es la propiedad.
--
--		Ejemplo: existe [0,1,2,3] [4,1,2,6] odd
--		retorna True.
-- ----------------------------------------------------
existe :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
existe xs ys p = and [p i ys | i <- xs]
       





