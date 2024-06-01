-- task 1
dup_recur :: [a] -> [a]
dup_recur [] = []
dup_recur (x:xs)= [x, x] ++ dup_recur(xs)

dup_fold :: [a] -> [a]
dup_fold xs = foldr (\x acc -> [x, x] ++ acc) [] xs
-- dup_fold xs = foldl (\acc x -> acc ++ [x, x]) [] xs
-- вы сказали в конец списка лучше не добавлять, поэтому foldr лучше будет, думаю



-- task 2
data Peano = Zero | Succ Peano

peanoToInt :: Peano -> Int
peanoToInt Zero = 0
peanoToInt (Succ x) = 1 + (peanoToInt x)
