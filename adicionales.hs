import Data.List (nub)

multiConjunto :: Eq a => [a] -> [(a, Int)]
multiConjunto xs = nub' [(x, cantOcu xs x) | x <- xs] 
    where cantOcu ys x = length [y | y <- ys, x == y]



nub' :: Eq a => [a] -> [a] --Toma una lista y devuelve otra sin elementos duplicados
nub' [] = []
nub' [x] = [x]
nub' (x:y:ys)  
            | x == y = nub' (y:ys)
            | otherwise = x:nub' (y:ys) 


--Matriz infinita
diag :: (Num b, Enum b) => b -> [(b, b)]
diag n = [(n-i, i) | i <- [0.. n]]

allPares :: [(Integer, Integer)]
allPares = concat [diag n | n <- [0..]]
    where   
        concat :: [[a]] -> [a]
        concat [] = []
        concat (x:xs) = x ++ concat xs



