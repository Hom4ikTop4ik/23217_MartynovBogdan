{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse on/on" #-}
import SQLHSSugar
import DBReader

-- CATEGORY:     WARE,    CLASS
-- MANUFACTURER: BILL_ID, COMPANY
-- MATERIAL:     BILL_ID, WARE,   AMOUNT
-- PRODUCT:      BILL_ID, WARE,   AMOUNT, PRICE

main = readDB' defaultDBName >>= task_5'1
da input = readDB' defaultDBName >>= input

-- TASK 5.1
-- For the query in the task 2.3 write the plan as efficient as possible.
-- 2.3: Get all the unique wares in alphabetical order that can be produced from wares in Mineral category.


-- SELECT DISTINCT PRODUCT.WARE--, MATERIAL.WARE
-- FROM PRODUCT
-- INNER JOIN MATERIAL
--   ON MATERIAL.BILL_ID == PRODUCT.BILL_ID
-- INNER JOIN CATEGORY
--	 ON CATEGORY.WARE == MATERIAL.WARE
-- WHERE CATEGORY.CLASS == 'Mineral'

task_5'1 :: (Named Table, Named Table, Named Table, Named Table) -> IO ()
task_5'1 (categories, manufacturers, materials, products) = do
    test "task 5.1" myTask
  
    where
      test msg p = do
        putStrLn $ "===== execute " ++ msg ++ " ====="
        -- putStrLn . debugTable $ p & enumerate
        printResult $ p & enumerate
    
      myTask = 
        -- PRODUCT 
        -- JOIN MATERIAL 
        --   ON MATERIAL.BILL_ID == PRODUCT.BILL_ID
        products // "pr0" `njoin` materials // "mat0" `on` "pr0.BILL_ID" `jeq` "mat0.BILL_ID"
        -- JOIN CATEGORY 
        --   ON CATEGORY.WARE == MATERIAL.WARE
        `njoin` categories // "cat0" `on` "mat0.WARE" `jeq` "cat0.WARE"
        -- FILTER cat0.CLASS = 'Mineral'
        `wher` col "cat0.CLASS" `eq` str "Mineral"
        -- MAP (pr0.WARE)
        `select` ["pr0.WARE"]
        -- DISTINCT
        & distinct 

    -- -- SQLHSExample.hs
    -- -- MANUFACTURER NL_JOIN PRODUCT ON m.BILL_ID=p.BILL_ID
    -- manufacturers // "m" `njoin` products // "p" `on` "m.BILL_ID" `jeq` "p.BILL_ID"
    -- -- -> NL_JOIN CATEGORY ON c.WARE=p.WARE
    -- `njoin` categories // "c" `on` "p.WARE" `jeq` "c.WARE"
    -- -- -> FILTER c.CLASS='Raw food'
    -- `wher` col "CLASS" `eq` str "Raw food"




-- Write the query that finds all the triplets of bill_id, material and product where the material is from the
-- “Mineral” category and the product is from the “Stuff” category. The result must be ordered by bill_id and
-- limited to the first 50 triplets. Write the plan as efficient as possible.
task_5'2 :: (Named Table, Named Table, Named Table, Named Table) -> IO ()
task_5'2 (categories, manufacturers, materials, products) = do
    test "task 5.2" myTask
  
    where
      test msg p = do
        putStrLn $ "===== execute " ++ msg ++ " ====="
        -- putStrLn . debugTable $ p & enumerate
        printResult $ p & enumerate
    
      myTask = 
        -- PRODUCT prod
        -- JOIN MATERIAL mat 
        --   ON mat.BILL_ID == prod.BILL_ID
        products // "prod" `njoin` materials // "mat" `on` "prod.BILL_ID" `jeq` "mat.BILL_ID"
        -- JOIN CATEGORY catProd
        --   ON catProd.WARE == prod.WARE
        `njoin` categories // "catProd" `on` "prod.WARE" `jeq` "catProd.WARE"
        -- JOIN CATEGORY catMat
        --   ON catMat.WARE == mat.WARE
        `njoin` categories // "catMat" `on` "mat.WARE" `jeq` "catMat.WARE"
        -- FILTER catMat.CLASS = 'Mineral'
        `wher` col "catMat.CLASS" `eq` str "Mineral"
        -- FILTER catProd.CLASS = 'Stuff'
        `wher` col "catProd.CLASS" `eq` str "Stuff"
        -- -> SORT_BY p.WARE
        `orderby` ["prod.BILL_ID":asc]
        
        `select` ["prod.BILL_ID", "prod.WARE", "mat.WARE"]
        -- DISTINCT
        & distinct 
        -- TAKE 50
        & limit 0 20





-- For the query in the task 2.8 write the plan as efficient as possible.
-- 2.8:
-- Get all the unique companies in alphabetical order implementing production chains. The production
-- chain is at least two subsequent bills of materials when the first bill producing ware that is in use as material
-- in the second bill. Example of such chain in terms of wares is Grain->Meat cow->Meat.
-- Найди компании, которые организовали производственные цепочки (от 2 конвейеров). Например, Grain->Meat cow->Meat
task_5'3 :: (Named Table, Named Table, Named Table, Named Table) -> IO ()
task_5'3 (categories, manufacturers, materials, products) = do
    test "task 5.3" myTask
  
    where
      test msg p = do
        putStrLn $ "===== execute " ++ msg ++ " ====="
        -- putStrLn . debugTable $ p & enumerate
        printResult $ p & enumerate
    
      myTask = 
        -- SELECT DISTINCT m1.COMPANY
        -- -- ищем компании, которые производят продукты ОДИН
        -- FROM MANUFACTURER m1
        -- INNER JOIN PRODUCT prod1
        --     ON prod1.BILL_ID == m1.BILL_ID
        -- -- смотрим на список материалов ОДИН для этого продуктов ОДИН
        -- INNER JOIN MATERIAL mat1 
	    --     ON mat1.BILL_ID == m1.BILL_ID
        -- -- подключаем список чеков, в которых материалы ОДИН производятся
        -- INNER JOIN PRODUCT prod2
	    --     ON prod2.WARE == mat1.WARE
        -- -- узнаём какие компании производят материалы ОДИН / продукты ДВА
        -- -- подключаем только те, что производят продукты ОДИН
        -- INNER JOIN MANUFACTURER m2
	    --     ON m2.BILL_ID == prod2.BILL_ID AND m2.COMPANY == m1.COMPANY
	    -- -- узнаём материалы ДВА для продукта ДВА
        -- INNER JOIN MATERIAL mat2
	    --     ON mat2.BILL_ID == m2.BILL_ID
        -- -- подключаем список чеков, в которых материалы ДВА производятся
        -- INNER JOIN PRODUCT prod3
	    --     ON prod3.WARE == mat2.WARE
        -- -- узнаём какие компании производят материалы ОДИН / продукты ДВА
        -- -- подключаем только те, что производят продукты ОДИН
        -- INNER JOIN MANUFACTURER m3
	    --     ON m3.BILL_ID == prod3.BILL_ID AND m3.COMPANY == m2.COMPANY
        -- ORDER BY m1.COMPANY ASC;
        
        -- "Завод 3"
        -- (manufacturers `orderby` ["BILL_ID":asc]) // "m3"
        manufacturers // "m3" 
        -- + товар 3 (конечный), который он производит
        -- `mjoin` (products `orderby` ["BILL_ID":asc]) // "prod3"
        `njoin` products // "prod3" 
            `on` "m3.BILL_ID" `jeq` "prod3.BILL_ID"
        -- + материалы 3 для товара 3
        -- `mjoin` (materials `orderby` ["BILL_ID":asc]) // "mat3" 
        `njoin` materials // "mat3" 
            `on` "m3.BILL_ID" `jeq` "mat3.BILL_ID"
        -- + товар 2 (средний) (он же материал 3)
        `njoin` (products `orderby` ["BILL_ID":asc]) // "prod2" 
            `on` "mat3.WARE" `jeq` "prod2.WARE"
        -- + "завод 2"
        -- `mjoin` (manufacturers `orderby` ["BILL_ID":asc]) // "m2"
        `njoin` manufacturers // "m2"
            `on` "prod2.BILL_ID" `jeq` "m2.BILL_ID"
        -- + материал 2 (он же товар 1)
        -- `mjoin` (materials `orderby` ["BILL_ID":asc]) // "mat2" 
        `njoin` materials // "mat2" 
            `on` "m2.BILL_ID" `jeq` "mat2.BILL_ID"
        -- + товар 1 (он же материал 2)
        -- `njoin` (products `orderby` ["BILL_ID":asc]) // "prod1" 
        `njoin` products // "prod1" 
            `on` "mat2.WARE" `jeq` "prod1.WARE"
        -- + "завод 1"
        -- `mjoin` (manufacturers `orderby` ["BILL_ID":asc]) // "m1" 
        `njoin` manufacturers // "m1" 
            `on` "prod1.BILL_ID" `jeq` "m1.BILL_ID"

        -- проверить, что это цепочка одной компании, а не разные производства
        `wher` col "m1.COMPANY" `eq` col "m2.COMPANY"
        `wher` col "m2.COMPANY" `eq` col "m3.COMPANY"

        -- SORT_BY p.WARE
        `orderby` ["m3.COMPANY":asc]

        `select` ["m3.COMPANY"]
        -- DISTINCT
        & distinct 