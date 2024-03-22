-- Чётное ли число? (функция even уже есть в Haskell, поэтому пишем even'; апостроф - просто часть названия)
even' :: Int -> Bool
even' a = (a `mod` 2) == 0

-- Нечётное ли число?
odd' :: Int -> Bool
odd' a = (a `mod` 2) /= 0

-- посчитать N-ное число Фибоначчи
fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib(n-1) + fib(n-2)

-- посчитать сумму нечётных чисел Фибоначи
sumOddFib :: Int -> Int
sumOddFib 1 = 1
sumOddFib n | odd'(fib(n)) = sumOddFib(n-1) + fib(n)
            | otherwise = sumOddFib(n-1)

-- magic number
magicHelp :: Int -> Int
magicHelp n | n < 10 = n
            | otherwise = (n `mod` 10) + (magicNum(n `div` 10))

magicNum :: Int -> Int
magicNum n | magicHelp(n) < 10 = magicHelp(n)
           | otherwise = magicHelp(magicHelp(n))
