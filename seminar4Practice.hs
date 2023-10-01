-- Задача 1 (создать бесконечный список из чисел Фибоначчи, кратных 5)
-- почистать N-ное число Фибоначчи
fibSlow :: Int -> Int
fibSlow 1 = 1
fibSlow 2 = 1
fibSlow n = fibSlow(n-1) + fibSlow(n-2)

-- числа Фибоначчи, кратные 5
fibSlowSet5 = [fibSlow(x) | x <- [1..], fibSlow(x) `mod` 5 == 0]

-- для "глазной" отладки
fibSlowSet = [fibSlow(x) | x <- [1..] ]

-- так как Haskell "умный" и оч ленивый язык, то он запоминает всё что подсчитывал
-- поэтому при повторном вызове "take 7 fibSet5" Haskell выводит мгновеннно
-- (берёт значения из кэша, наверное)



-- Задача 2 ("Найти периметр треугольника")
calcTrePerim :: [(Double, Double)] -> Double
calcTrePerim [(a1,b1), (a2,b2), (a3,b3)] = (sqrt((a1-a2)*(a1-a2)+(b1-b2)*(b1-b2))) + (sqrt((a1-a3)*(a1-a3)+(b1-b3)*(b1-b3))) + (sqrt((a2-a3)*(a2-a3)+(b2-b3)*(b2-b3)))



-- Задача 3 ("Равны ли все элементы в списке?")
checkAllEq :: Eq a => [a] -> Bool
checkAllEq [] = True
checkAllEq [x] = True
checkAllEq (x1:x2:xs) = if (x1 == x2) then checkAllEq(x2:xs) else False



-- Задача 4 ("Найти минимальное расстояние меж точками")
-- Расстояние между точками или же длина отрезка
lenLine :: (Double, Double) -> (Double, Double) -> Double
lenLine (a, b) (c, d) = sqrt(x*x + y*y)
                        where x = a-c; y = b-d

-- своя функция выбора минимального
-- min :: Double -> Double -> Double
-- min a b = if a < b then a else b

-- внешний цикл
for1 :: [(Double, Double)] -> Int -> Double -> Double
for1 arr i minimum | (i < 0)  = minimum
                   | (i >= 0) = for1 arr  (i-1)  (min minimum  (for2 arr (i-1) minimum i))

-- цикле в цикле
for2 :: [(Double, Double)] -> Int -> Double -> Int -> Double
for2 arr j minimum i | (j <= 0)  = minimum
                     | (j > 0) = for2 arr  (j-1)  (min minimum  (lenLine (arr!!(j-1)) (arr!!(i-1))))  i

-- основная функция
lenMin :: [(Double, Double)] -> Double
lenMin arr = for1 arr (length arr) 999999999.0



-- Задача 5 (калькулятор по командам: x++; x--; x*2; x/2 if >0; √x)
-- ↓ выполняет по 1 команде
computeHelper :: String -> Double -> Double
computeHelper s p | (s == "inc")             = p + 1
                  | (s == "dec")             = p - 1
                  | (s == "sqrt")            = sqrt p
                  | (s == "double")          = p * 2
                  | (s == "halveIfPositive") = if (p > 0) then p / 2 else p
                  | otherwise = error "---IDK this command---"

compute :: [String] -> Double -> Double
compute [] p = p
--       ↓ отсекаем первую команду    ↓ считаем p, следуя первой команде из списка
compute (x:xs) p = compute xs (computeHelper x p)



-- Задача 1* (ускорить подсчёт чисел Фибоначчи)
fib' :: Int -> Integer -> Integer -> Integer
fib' n a b | (n == 0)  = a -- нулевое число Фибоначчи = 0
           | (n == 1)  = b --  первое число Фибоначчи = 1, второе тоже
           | otherwise = fib' (n-1) b (a+b)

fib :: Int -> Integer
fib n = fib' n 0 1

-- числа Фибоначчи, кратные 5
fibSet5 = [fib(x) | x <- [1..], fib(x) `mod` 5 == 0]

-- для "глазной" отладки
fibSet = [fib(x) | x <- [1..] ]