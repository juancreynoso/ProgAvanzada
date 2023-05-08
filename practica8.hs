
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

-- Ejercicio 3
paraTodo :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
paraTodo xs ys 