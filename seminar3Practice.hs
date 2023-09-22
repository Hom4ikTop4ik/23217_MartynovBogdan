-- Zadacha 1
res1 :: (Double -> Double) -> (Double -> Double) -> Double -> Double
res1 f g p = max (f(p)) (g(p))

e :: Double
e = exp 1

-- Zadacha 2
res2 :: (Double -> Double) -> Double -> Double
res2 f p = res1 f (\x -> e**p) p

-- Zadacha 3
res3 :: (Double -> Double) -> Double -> Double -> Double
res3 f p n | n < -1 = error "pls, use n >= 0"
           | n == 0 = p
           | otherwise = res3 f (f p) (n-1)