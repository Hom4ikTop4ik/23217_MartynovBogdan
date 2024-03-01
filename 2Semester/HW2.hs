-- da
import Control.Monad.State


-- TASK 1

-- от ChatGPT
-- -- | Определение состояния
-- type FactState = (Int, Int)

-- -- | Монада State для вычисления факториала
-- fact' :: State FactState Int
-- fact' = do
--   -- Извлекаем текущий шаг и аккумулятор из состояния
--   (step, acc) <- get

--   -- Проверяем, если шаг равен 0, возвращаем аккумулятор
--   if step == 0
--     then return acc
--     else do
--       -- Уменьшаем шаг на 1
--       let newStep = step - 1

--       -- Обновляем состояние с новым шагом
--       put (newStep, acc * step)

--       -- Рекурсивно вызываем fact'
--       fact'

fact' :: State (Int, Int) Int
fact' = do
    --                если возвращать step вместо step-1, в конце умножиться на 0
    (step, answer)   <- state $ \(step, acc) -> ((step-1, acc), (step-1, acc*step))

    -- "<="  if user isn't smart😁 and puts negative number into the func
    if step <= 0 
        then return answer
        else do
            fact'

fact :: Int -> Int
fact n = evalState fact' (n, 1)



-- TASK 2 (сделал сам, на основе TASK 1)
-- (step, n1, n2)
fibb' :: State (Int, Int, Int) Int
fibb' = do
    (step, a, b) <- state $ \(step, a1, a2) -> ((step, a1, a2), (step-1, a2, a1+a2))
    -- a    <- state $ \(step, a1, a2) -> (  a1, (step,   a1, a2))
    -- b    <- state $ \(step, a1, a2) -> (  a2, (step,   a1, a2))
    -- step <- state $ \(step, a1, a2) -> (step, (step-1, a2, a1+a2))

    -- "<"   if user isn't smart😁 and puts negative number into the func
    if step < 0 
        then return (-1)
        else
            if step == 0
                then return b
                else do
                    fibb'


fibb :: Int -> Int
fibb n = evalState fibb' (n, 1, 0)

-- проверка чисел:
da = [0,1,2,3,4,5,6,7,8,9]
ne :: [Int]
ne = foldl (\acc x -> acc ++ [fibb x]) [] da



-- TASK 3
data BinTree a =
    Nil |
    -- Node (BinTree a) a (BinTree a) 
    Node {
        left :: BinTree a,
        value :: a,
        right :: BinTree a
    }

-- Ваш вывод из 10 семинара 1-ого семестра
instance Show a => Show (BinTree a) where
    show :: Show a => BinTree a -> String
    show = show0 0 where
        show0 _ Nil = "Nil"
        show0 lvl Node{left=l, right=r, value=v} =
            "Node (v = " ++ show v ++ ")\n" ++
            replicate lvl '\t' ++ "l=" ++ show0 (lvl+1) l ++ "\n" ++
            replicate lvl '\t' ++ "r=" ++ show0 (lvl+1) r ++ "\n"
            -- replicate lvl '\t' — печатает lvl раз символ '\t'

numberTree' :: BinTree () -> State Integer (BinTree Integer)
numberTree' Nil = return Nil
numberTree' (Node left cur right) = do
    -- рекурсивно получили левое поддерево
    l <- numberTree' left
    -- передать num, после чего увеличить его
    c <- state $ \(num) -> (num, num+1)
    -- рекурсивно получили правое поддерево
    r <- numberTree' right
    -- вернули "склееное" дерево: левая ветка, основание, правая ветка
    return (Node l c r)

numberTree :: BinTree () -> BinTree Integer
numberTree tree = evalState (numberTree' tree) 0 -- нумерация с 0, мы ведь программисты
