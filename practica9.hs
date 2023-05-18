

-- f.xs.ys determina si ys es una subsecuencia de xs.
--     f :: [a] -> [a] -> Bool
--     f. xs.ys = < Existe as, bs :: as ++ ys ++ bs = xs >

esSub :: Eq a => [a] -> [a] -> Bool
esSub xs ys = or [ys == take n xs || ys == drop n xs | n <- [0..length xs]]

-- f.xs.ys determina si ys es una subsecuencia final de xs.

--     f :: [a] -> [a] -> Bool
--     f. xs.ys = < Existe zs :: zs ++ ys = xs > 

esSubFinal :: Eq a => [a] -> [a] -> Bool
esSubFinal xs ys = or [ys == take n xs || ys == drop n xs | n <- [0..length xs]]


prodPrimos :: [Int] -> Int
prodPrimos xs = product [x | x <- xs, esPrimo x]
    where 
        esPrimo 1 = False
        esPrimo n = and [mod n i /= 0 | i<-[2..(n-1)]]


split2 :: [a] -> [([a], [a])]
split2 xs = [(take n xs, drop n xs) | n <- [0..length xs]]