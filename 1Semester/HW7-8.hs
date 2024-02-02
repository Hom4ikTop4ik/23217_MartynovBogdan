import Data.Array

newtype Matrix = Matrix (Array (Int, Int) Double)
--   deriving Show -- для вывода в консоль

----------------------------------------------------------------------------------------------------

-- ======
--                                                        --                                          ▓▓▓▓▓
--  ▓    ▓   ▓   ▓        ▓▓▓▓   ▓    ▓   ▓▓▓▓   ▓    ▓   --   ▓▓▓▓▓    ▓▓     ▓▓▓▓   ▓    ▓         ▓     ▓
--  ▓▓  ▓▓    ▓ ▓        ▓       ▓    ▓  ▓    ▓  ▓    ▓   --     ▓     ▓  ▓   ▓       ▓   ▓                ▓
--  ▓ ▓▓ ▓     ▓          ▓▓▓▓   ▓▓▓▓▓▓  ▓    ▓  ▓    ▓   --     ▓    ▓    ▓   ▓▓▓▓   ▓▓▓▓            ▓▓▓▓▓
--  ▓    ▓     ▓              ▓  ▓    ▓  ▓    ▓  ▓ ▓▓ ▓   --     ▓    ▓▓▓▓▓▓       ▓  ▓  ▓                 ▓
--  ▓    ▓     ▓         ▓    ▓  ▓    ▓  ▓    ▓  ▓▓  ▓▓   --     ▓    ▓    ▓  ▓    ▓  ▓   ▓          ▓     ▓
--  ▓    ▓     ▓          ▓▓▓▓   ▓    ▓   ▓▓▓▓   ▓    ▓   --     ▓    ▓    ▓   ▓▓▓▓   ▓    ▓          ▓▓▓▓▓
-- ====== 

instance Show Matrix where
    show :: Matrix -> String
    show mtx = " \\ j\t" ++ (foldl (\acc x -> acc ++ show(x) ++ "\t") "" [snd(fst(bounds arr)) .. snd(snd(bounds arr))]) ++ "\ni \\" ++ fst(matrix_foldl (\(acc, i) ((_, snd_ind), x) -> (if snd_ind == start then (acc ++ "\n" ++ (show i) ++ "\t" ++ show(x) ++ "\t", i+1) else (acc ++ (show x) ++ "\t", i))) ("", fst(fst(bounds arr))) mtx) where start = snd (fst (bounds arr)); Matrix arr = mtx

----------------------------------------------------------------------------------------------------

--                                                        --                         ▓▓▓▓▓▓▓
--  ▓    ▓   ▓   ▓       ▓▓▓▓▓▓   ▓▓▓▓    --   ▓▓▓▓▓    ▓▓     ▓▓▓▓   ▓    ▓         ▓
--  ▓▓  ▓▓    ▓ ▓        ▓       ▓    ▓   --     ▓     ▓  ▓   ▓       ▓   ▓          ▓
--  ▓ ▓▓ ▓     ▓         ▓▓▓▓▓   ▓    ▓   --     ▓    ▓    ▓   ▓▓▓▓   ▓▓▓▓            ▓▓▓▓▓
--  ▓    ▓     ▓         ▓       ▓  ▓ ▓   --     ▓    ▓▓▓▓▓▓       ▓  ▓  ▓                 ▓
--  ▓    ▓     ▓         ▓       ▓   ▓    --     ▓    ▓    ▓  ▓    ▓  ▓   ▓          ▓     ▓
--  ▓    ▓     ▓         ▓▓▓▓▓▓   ▓▓▓ ▓   --     ▓    ▓    ▓   ▓▓▓▓   ▓    ▓          ▓▓▓▓▓

instance Eq Matrix where
   (==) :: Matrix -> Matrix -> Bool
   mtx1 == mtx2   | matrix_size mtx1 == matrix_size mtx2 = matrix_foldl (\acc ((i, j), x) -> if acc == True then (x == (mtx2 !!! (i, j))) else False) True mtx1
                  | otherwise = False

----------------------------------------------------------------------------------------------------
--                                                                                            ▓▓▓▓▓
--  ▓    ▓   ▓   ▓       ▓    ▓  ▓    ▓  ▓    ▓   --   ▓▓▓▓▓    ▓▓     ▓▓▓▓   ▓    ▓         ▓     ▓
--  ▓▓  ▓▓    ▓ ▓        ▓▓   ▓  ▓    ▓  ▓▓  ▓▓   --     ▓     ▓  ▓   ▓       ▓   ▓          ▓
--  ▓ ▓▓ ▓     ▓         ▓ ▓  ▓  ▓    ▓  ▓ ▓▓ ▓   --     ▓    ▓    ▓   ▓▓▓▓   ▓▓▓▓           ▓▓▓▓▓▓
--  ▓    ▓     ▓         ▓  ▓ ▓  ▓    ▓  ▓    ▓   --     ▓    ▓▓▓▓▓▓       ▓  ▓  ▓           ▓     ▓
--  ▓    ▓     ▓         ▓   ▓▓  ▓    ▓  ▓    ▓   --     ▓    ▓    ▓  ▓    ▓  ▓   ▓          ▓     ▓
--  ▓    ▓     ▓         ▓    ▓   ▓▓▓▓   ▓    ▓   --     ▓    ▓    ▓   ▓▓▓▓   ▓    ▓          ▓▓▓▓▓

instance Num Matrix where
   (+) :: Matrix -> Matrix -> Matrix
   mtx1 + mtx2 = make_matrix (-1, -1) (matrix_foldl (\acc ((i, j), x) -> acc ++ [((i, j), x + (mtx2 !!! (i, j)))]) [] mtx1)

   -- (*) :: Matrix -> Matrix -> Matrix
--  number of column (кол-во столбцов) == number of rows (кол-во строк)
   mtx1 * mtx2 | snd(snd(bounds arr1)) /= fst(snd(bounds arr2)) = make_matrix (0,0) [] -- return empty matrix
               | otherwise = make_matrix (-1, -1) ([((ii, jj), sum (zipWith (*) [mtx1 !!! (ii, kk) | kk<-[0..both]] [mtx2 !!! (kk, jj) | kk<-[0..both]])) | ii<-[0..i], jj<-[0..j]]) where i = fst(snd(bounds arr1)); j = snd(snd(bounds arr2)); both = snd(snd(bounds arr1)); Matrix arr1 = mtx1; Matrix arr2 = mtx2

   negate :: Matrix -> Matrix
   negate mtx = matrixMap (\(_, e) -> (-1) * e) mtx

   abs :: Matrix -> Matrix
   abs mtx = matrixMap (\(_, e) -> (abs e)) mtx

   signum :: Matrix -> Matrix
   signum mtx = matrixMap (\(_, e) -> if e < 0 then -1 else if e > 0 then 1 else 0) mtx

   fromInteger :: Integer -> Matrix
   fromInteger = undefined
   -- fromInteger num = make_matrix (num, num) [((i, j), num) | i <- [0..(num-1)], j <- [0..(num-1)]] 
 
----------------------------------------------------------------------------------------------------

-- примеры
example1 :: [((Int, Int), Double)]
example1 = [((0,0), 1), ((0,1), -2), ((0,2), 3), -- строка 1: j = 1
            ((1,0), 4), ((1,1), 0),  ((1,2), 6), -- строка 2: j = 2
            ((2,0), -7), ((2,1), 8), ((2,2), 9)] -- строка 3: j = 3
                                                 -- всего строк: i = 3

example2 :: [((Int, Int), Double)]
example2 = [((0,0), 13), ((0,1), 0),
            ((1,0), 0), ((1,1), 12)]

my_example1 :: [((Int, Int), Double)]
my_example1 = [((0,0), 1), ((0,1), 2),
            ((1,0), 3), ((1,1), 4)]

my_example2 :: [((Int, Int), Double)]
my_example2 = [((0,0), 1), ((0,1), 3),
            ((1,0), 2), ((1,1), 4)]

my_example3 :: [((Int, Int), Double)]
my_example3 = [((0, 0), 1), ((0, 1), 2), ((0, 2), 3), ((0, 3), 4), 
               ((1, 0), 5), ((1, 1), 6), ((1, 2), 7), ((1, 3), 8), 
               ((2, 0), 9), ((2, 1), 10), ((2, 2), 11), ((2, 3), 12)]

my_example4 :: [((Int, Int), Double)]
my_example4 = [((0, 0), 1), ((0, 1), 2), ((0, 2), 3), ((0, 3), 4), ((0, 4), 5), ((0, 5), 6), ((0, 6), 7), 
               ((1, 0), 8), ((1, 1), 9), ((1, 2), 10), ((1, 3), 11), ((1, 4), 12), ((1, 5), 13), ((1, 6), 14), 
               ((2, 0), 15), ((2, 1), 16), ((2, 2), 17), ((2, 3), 18), ((2, 4), 19), ((2, 5), 20), ((2, 6), 21),
               ((3, 0), 22), ((3, 1), 23), ((3, 2), 24), ((3, 3), 25), ((3, 4), 26), ((3, 5), 27), ((3, 6), 28)]

m1 :: Matrix
m1 = make_matrix (-1, -1) example1
m2 :: Matrix
m2 = make_matrix (2, 2) example2

my_m1 :: Matrix
my_m1 = make_matrix (2, 2) my_example1
my_m2 :: Matrix
my_m2 = make_matrix (2, 2) my_example2

my_m3 :: Matrix
my_m3 = make_matrix (-1, -1) my_example3
my_m4 :: Matrix
my_m4 = make_matrix (-1, -1) my_example4


-- ======
--                                            ▓
--   ▓▓▓▓▓    ▓▓     ▓▓▓▓   ▓    ▓           ▓▓
--     ▓     ▓  ▓   ▓       ▓   ▓           ▓ ▓
--     ▓    ▓    ▓   ▓▓▓▓   ▓▓▓▓              ▓
--     ▓    ▓▓▓▓▓▓       ▓  ▓  ▓              ▓
--     ▓    ▓    ▓  ▓    ▓  ▓   ▓             ▓
--     ▓    ▓    ▓   ▓▓▓▓   ▓    ▓          ▓▓▓▓▓
-- ======

-- main function ↓
make_matrix :: (Int, Int) -> [((Int, Int), Double)] -> Matrix
-- You can use indexes (-1, -1) and function will calculate size of array itself
-- функция сама подсчитает размеры, если ты укажешь раззмеры (-1, -1)
make_matrix (-1, -1) list = Matrix (array ((0, 0), (x, y)) list) where (x, y) = get_last_indexes list
make_matrix (i, j) list = Matrix (array ((0, 0), (i - 1, j - 1)) list)


get_last_indexes :: [((Int, Int), Double)] -> (Int, Int)
get_last_indexes arr = ((get_i arr) - 1, (get_j arr) - 1)


-- number of column (string lenght) (НЕ ИНДЕКС: if we have 1 elem, lenght will 1)
get_j :: [((Int, Int), Double)] -> Int
get_j arr = foldl (\acc ((a, _), _) -> if (a == helper) then acc + 1 else acc) 0 arr where helper = fst(fst (head arr))


-- number of rows (тоже НЕ ИНДЕКС)
get_i :: [((Int, Int), Double)] -> Int
get_i arr = foldl (\acc ((_, b), _) -> if (b == helper) then acc + 1 else acc) 0 arr where helper = snd(fst(head arr))


-- get matrix elem
(!!!) :: Matrix -> (Int, Int) -> Double
Matrix mtx !!! (a, b) = mtx ! (a, b)


-- get matrix sizes (NOT last indexes)
matrix_size :: Matrix -> (Int, Int)
matrix_size (Matrix mtx) = (c-a+1, d-b+1) where ((a,b), (c,d)) = bounds mtx


matrix_indices :: Matrix -> [(Int, Int)]
matrix_indices (Matrix mtx) = indices mtx

----------------------------------------------------------------------------------------------------




----------------------------------------------------------------------------------------------------

-- ======
--                                          ▓▓▓▓▓
--   ▓▓▓▓▓    ▓▓     ▓▓▓▓   ▓    ▓         ▓     ▓
--     ▓     ▓  ▓   ▓       ▓   ▓                ▓
--     ▓    ▓    ▓   ▓▓▓▓   ▓▓▓▓            ▓▓▓▓▓
--     ▓    ▓▓▓▓▓▓       ▓  ▓  ▓           ▓
--     ▓    ▓    ▓  ▓    ▓  ▓   ▓          ▓
--     ▓    ▓    ▓   ▓▓▓▓   ▓    ▓         ▓▓▓▓▓▓▓
-- ======

type MtxElem = ((Int, Int), Double)

matrix_foldl :: (b -> MtxElem -> b) -> b -> Matrix -> b
matrix_foldl func acc mtx = foldl func acc (toList mtx)

toList :: Matrix -> [MtxElem]
toList (Matrix mtx) = fst(foldl (\(acc, ind:indexes) x -> ((acc ++ [(ind, x)]), indexes)) ([], indices mtx) (elems mtx))

-- Example:
matrixSumElems mtx = matrix_foldl (\acc (_, e) -> acc + e) 0 mtx

-- map
matrixMap :: (MtxElem -> Double) -> Matrix -> Matrix
matrixMap func mtx = make_matrix ((-1), (-1)) (matrix_foldl (\acc (indexes, x) -> acc ++ [(indexes, func(indexes, x))]) [] mtx)

matrixMulScalar x mtx = matrixMap (\(_, e) -> x * e) mtx

----------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------

-- ======
--                                         ▓
--   ▓▓▓▓▓    ▓▓     ▓▓▓▓   ▓    ▓         ▓    ▓
--     ▓     ▓  ▓   ▓       ▓   ▓          ▓    ▓
--     ▓    ▓    ▓   ▓▓▓▓   ▓▓▓▓           ▓▓▓▓▓▓▓
--     ▓    ▓▓▓▓▓▓       ▓  ▓  ▓                ▓
--     ▓    ▓    ▓  ▓    ▓  ▓   ▓               ▓
--     ▓    ▓    ▓   ▓▓▓▓   ▓    ▓              ▓
-- ======

transpose :: Matrix -> Matrix
transpose mtx = make_matrix (-1, -1) (matrix_foldl (\acc ((i, j), _) -> acc ++ [((i, j), mtx !!! (j, i))]) [] mtx)

----------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------

-- ======
-- task 7
-- ======

det2 :: Matrix -> Double
det2 mtx = (mtx !!! (0,0)) * (mtx !!! (1,1)) - (mtx !!! (0,1)) * (mtx !!! (1,0))

det3 :: Matrix -> Double
det3 mtx = (mtx !!! (0,0))*(mtx !!! (1,1))*(mtx !!! (2,2)) + 
           (mtx !!! (0,1))*(mtx !!! (1,2))*(mtx !!! (2,0)) + 
           (mtx !!! (0,2))*(mtx !!! (1,0))*(mtx !!! (2,1)) - 
           (mtx !!! (0,2))*(mtx !!! (1,1))*(mtx !!! (2,0)) - 
           (mtx !!! (0,0))*(mtx !!! (1,2))*(mtx !!! (2,1)) - 
           (mtx !!! (0,1))*(mtx !!! (1,0))*(mtx !!! (2,2))
   
----------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------

-- ======
-- task 8
-- ======

isDiagonal :: Matrix -> Bool
isDiagonal mtx = matrix_foldl (\acc ((i, j), x) -> if i == j then acc else if (mtx !!! (i, j)) == 0 then acc else False ) True mtx

isSymmetrical :: Matrix -> Bool
isSymmetrical mtx = matrix_foldl (\acc ((i, j), x) -> if i == j then acc else if (mtx !!! (i, j)) == (mtx !!! (j, i)) then acc else False ) True mtx
----------------------------------------------------------------------------------------------------
