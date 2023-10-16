import Text.XHtml (base)
-- Задача 1
reverse' :: [a] -> [a]
reverse' a = foldl (\acc x -> (x:acc)) [] a


-- Задача 2
onlyFuns :: [a] -> [a] -> [a]
onlyFuns [] acc = acc
onlyFuns [_] acc = acc
onlyFuns (x:y:arr) acc = onlyFuns arr (acc++[y])

evenOnly :: [a] -> [a]
evenOnly a = onlyFuns a [] 


-- Задача 3
for :: (Int, a) -> (Int -> Int) -> (Int -> Bool) -> (Int -> a -> a) -> a
for (i, a) incer ifer func = if (ifer i) then for ((incer i), (func i a)) (incer) (ifer) func else a

sum' :: Num a => [a] -> a
sum' lst = for (0, 0) (\i -> i+1) (\i -> i < length lst) (\i acc -> acc + lst!!i)

concatAll :: [String] -> String
concatAll strs = for (0, "") (\i -> i+1) (\i -> i < length strs) (\i acc -> acc ++ strs!!i)


-- Задача 4
decart :: [a] -> [b] -> [(a, b)]
decart a b = [(x, y) | x<-a, y<-b]


decart' :: [a] -> [b] -> [(a, b)]
decart' a b = decartHelperA a b []

decartHelperB :: [a] -> b -> [(a, b)] -> [(a, b)]
decartHelperB [] _ acc = acc
decartHelperB (x:xs) b acc = decartHelperB xs b ([(x, b)] ++ acc)

decartHelperA :: [a] -> [b] -> [(a, b)] -> [(a, b)]
decartHelperA _ [] acc = acc
decartHelperA a (x:xs) acc = decartHelperA a xs (decartHelperB a x [] ++ acc)


-- Задача 5
type BinaryRelation a = Eq a => [(a, a)]

refl :: Eq a => [a] -> BinaryRelation a -> Bool
refl [] b = True
refl (x:xs) b = if (elem (x, x) b) then refl xs b else False


sim :: Eq a => [a] -> BinaryRelation a -> Bool
sim a b = simHelper b b

simHelper :: (Eq a, Eq b) => [(a, b)] -> [(b, a)] -> Bool
simHelper [] b = True
simHelper ((x, y):xs) b = if (elem (y, x) b) then simHelper xs b else False


-- trans :: Eq a => [a] -> BinaryRelation a -> Bool
-- trans 