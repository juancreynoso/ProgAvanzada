prom :: (Num a, Num b) => [a] -> (a, b)
prom [] = (0,0)
prom (x:xs) = (x + a, 1 + b)
    where (a,b) = prom xs


promedio :: Fractional a => [a] -> a
promedio xs = fst (prom xs) / snd (prom xs)

elemIgu :: Eq a => [a] -> Bool
elemIgu [] = True
elemIgu (x:xs) = g xs x

g :: Eq a => [a] -> a -> Bool
g [] z = True
g (x:xs) z = z == x && g xs z