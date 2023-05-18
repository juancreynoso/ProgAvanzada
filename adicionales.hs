import Data.List (nub)

multiConjunto :: Eq a => [a] -> [(a, Int)]
multiConjunto xs = nub' [(x, cantOcu xs x) | x <- xs]
    where cantOcu ys x = length [y | y <- ys, x == y]



nub' :: Eq a => [a] -> [a] --Toma una lista y devuelve otra sin elementos duplicados
nub' [] = []
nub' [x] = [x]
nub' (x:xs)
            | x `elem` xs = nub' xs
            | otherwise = x:nub' xs


--Matriz infinita
diag :: (Num b, Enum b) => b -> [(b, b)]
diag n = [(n-i, i) | i <- [0.. n]]

allPares :: [(Integer, Integer)]
allPares = concat [diag n | n <- [0..]]
    where
        concat :: [[a]] -> [a]
        concat [] = []
        concat (x:xs) = x ++ concat xs


map' :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
map' f xs = foldl (\acc -> \x -> acc ++ [f x]) [] xs


filter' f xs = foldl (\acc x -> acc ++ [f x]) [] xs




