
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use splitAt" #-}


-- EJERCICIO 5

-- Ejercicio 1 (e)
prodPrimos :: [Int] -> Int
prodPrimos xs = product [x | x <- xs, esPrimo x]
    where 
        esPrimo 1 = False
        esPrimo n = and [mod n i /= 0 | i<-[2..(n-1)]]

-- Ejercicio 3 (c)
-- f.xs.ys determina si ys es una subsecuencia de xs.
--     f :: [a] -> [a] -> Bool
--     f. xs.ys = < Existe as, bs :: as ++ ys ++ bs = xs >

esSub :: Eq a => [a] -> [a] -> Bool
esSub xs ys = or [bs == ys | (as, bs, cs) <- split3 xs]


-- Ejercicio 3 (d)
-- f.xs.ys determina si ys es una subsecuencia final de xs.
--     f :: [a] -> [a] -> Bool
--     f. xs.ys = < Existe zs :: zs ++ ys = xs > 

esSubFinal :: Eq a => [a] -> [a] -> Bool
esSubFinal xs ys = or [ys == bs | (as, bs) <- split2 xs]

-- Ejercicio 5

-- < Max as, bs, cs : xs = as ++ bs ++ cs : sum bs >
-- Calcula el valor del subsegmento de suma máxima

maxSubSum :: [Int] -> Int
maxSubSum xs = maximum [sum bs | (as, bs, cs) <- split3 xs]



split2 :: [a] -> [([a], [a])]
split2 xs = [(take n xs, drop n xs) | n <- [0..length xs]]

split3 :: Eq a => [a] -> [([a], [a], [a])]
split3 xs = [(as, bs, cs) | (as, ys) <- split2 xs, (bs, cs) <- split2 ys]