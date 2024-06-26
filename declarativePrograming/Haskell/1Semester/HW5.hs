-- Задача 1
-- Через foldl
map' :: (a -> b) -> [a] -> [b]
map' f arr = foldl (\acc x -> acc ++ [(f x)]) [] arr
-- Через foldr
map'' :: (a -> b) -> [a] -> [b]
map'' f arr = foldr (\x acc -> (f x):acc ) [] arr
-- конкатенация как " x:xs " так и  " [x]++xs "
-- map'' f = foldr (\x acc -> [(f x)] ++ acc ) []


-- Задача 2
nub' :: Eq a => [a] -> [a]
nub' [] = [] -- если получили пустой список, фильтровать нечего — конец рекурсии
nub' (x:xs) = [x] ++ nub' (filter (/= x) xs) -- x ++ массив без этого x (рекурсивно НЕ берём каждый повтор)

union' :: Eq a => [a] -> [a] -> [a] -- a b >>> (a U b)
union' xs ys = nub' (xs ++ ys) -- 1) конкатенируем множества; 2) убираем повторы, так как во множестве нет повторяющихся элементов

intersection' :: Eq a => [a] -> [a] -> [a] -- a b >>> (a ∩ b)
intersection' xs ys = foldl (\res x -> res ++ (if (elem x ys) then [x] else [])) [] xs -- пробегаемся по xs: если x ∈ ys, добавим в res


-- Задача 3
-- ↓ выполняет по 1 команде
computeHelper :: String -> Double -> Double
computeHelper s p | (s == "inc")             = p + 1
                  | (s == "dec")             = p - 1
                  | (s == "sqrt")            = sqrt p
                  | (s == "double")          = p * 2
                  | (s == "halveIfPositive") = if (p > 0) then p / 2 else p
                  | otherwise = error "---IDK this command---"

-- функция, считающая 1 элемент
compute :: [String] -> Double -> Double
compute [] p = p
--       ↓ отсекаем первую команду    ↓ считаем p, следуя первой команде из списка
compute (x:xs) p = compute xs (computeHelper x p)

-- основная ф-ция
computeArr :: [String] -> [Double] -> [Double]
computeArr arr xs = map' (compute (cleaner arr)) xs -- примерним ф-цию compute к каждому элементу xs и вернём полученный список
--                                 ↑ он для безошибчного выполнения программы (убрать все несущ. команды)


-- Задача 4
cleaner :: [String] -> [String]
cleaner [] = []
cleaner (x:xs) = (  if (elem x ["inc", "dec", "sqrt", "double", "halveIfPositive"]) then [x] else []  ) ++ cleaner(xs)


-- Задача 5
-- Задача 5*
optimizer :: [String] -> [String]
--               ↓ после очистки так же? — это ответ ↓; после очистки другое? — ↓ рекурсивная чистка 
optimizer arr = y where (x:y) = reverse(foldl (\(first:acc) x -> if (((first, x) == ("inc", "dec")) || ((first, x) == ("dec", "inc"))) then acc else (x:(first:acc))) [""] arr)


-- Задача 6. ↓ данный массив точек    ↓ функция       ответ ↓ — массив из точек выше f 
bolshe :: [(Double, Double)] -> (Double -> Double) -> [(Double, Double)] 
--                                      ↓ точка выше — приписываем ↓, иначе ничего
bolshe points f = foldl (\res (x, y) -> if (f(x) > y) then res ++ [(x, y)] else res) [] points
