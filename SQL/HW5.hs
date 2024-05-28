{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse on/on" #-}
import SQLHSSugar
import DBReader

-- CATEGORY:     WARE,    CLASS
-- MANUFACTURER: BILL_ID, COMPANY
-- MATERIAL:     BILL_ID, WARE,   AMOUNT
-- PRODUCT:      BILL_ID, WARE,   AMOUNT, PRICE

-- task 5.1: 
--   merg:  0s - 11667 (12554 with ORDER BY)
--   hash:  0s - 12554 (12531 without ORDER BY)
--   join: .2s - result/32s - operations: 18'163'681
-- task 5.2: 
--   merg:  0s - 1021 (9330 with ORDER BY,  650 without ORDER)
--   hash:  0s - 9633 (650 without ORDER BY)
--   join: 25s - result/5s  - operations: 18'481'020
-- task 5.3:
--   merg: .2s - result/.2s - operations: 128168 (130457 with ORDER BY)
--   hash:  6s - result/1s  - operations: 3'141'580 (3'143'869 with ORDER BY)
--   merg: 💀🔪 - "Файл подкачки слишком мал" (ну да, 10ГБ ОЗУ + 25ГБ подкачки мало)


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
        -- subquery: SELECT category.WARE 
        --           where category.CLASS == 'Mineral'
        flatten 
        (
          (
            (
              categories // "cat0"
              `wher` col "cat0.CLASS" `eq` str "Mineral"
              `select` ["cat0.WARE"]
              & distinct
            )
            -- join material which category == 'Mineral'
            `hjoin` (materials `indexby` col "WARE") // "mat0" `on` col "cat0.WARE"
            -- join product made by 'Mineral' materials 
            `hjoin` (products `indexby` col "BILL_ID") // "pr0" `on` col "mat0.BILL_ID" 
          )
          `indexby` col "pr0.WARE"
        )
        `select` ["pr0.WARE"]
        & distinct 
      
      myTaskOrderBy = 
        -- subquery: SELECT category.WARE 
        --           where category.CLASS == 'Mineral'
        (
          (
            categories // "cat0"
            `wher` col "cat0.CLASS" `eq` str "Mineral"
            `select` ["cat0.WARE"]
            & distinct
          )
          -- join material which category == 'Mineral'
          `hjoin` (materials `indexby` col "WARE") // "mat0" `on` col "cat0.WARE"
          -- join product made by 'Mineral' materials 
          `hjoin` (products `indexby` col "BILL_ID") // "pr0" `on` col "mat0.BILL_ID" 
          `select` ["pr0.WARE"]
          & distinct 
        )
        `orderby` ["pr0.WARE":asc]
        `select` ["pr0.WARE"]




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
        flatten 
        (
          (
            (categories `wher` col "CLASS" `eq` str "Stuff") // "catProd"
            `hjoin` (products `indexby` col "WARE") // "prod" `on` col "catProd.WARE"

            `hjoin` (materials `indexby` col "BILL_ID") // "mat" `on` col "prod.BILL_ID"

            `hjoin` (
                      (categories `wher` col "CLASS" `eq` str "Mineral") 
                      `indexby` col "WARE"
                     ) // "catMat" `on` col "mat.WARE"
          )

          `indexby` col "prod.BILL_ID"
        )

        
        `select` ["prod.BILL_ID", "mat.WARE", "prod.WARE"]
        -- & distinct 
        & limit 0 50 -- skip 0 rows and take 50


      myTaskOrderBy = 
        (categories `wher` col "CLASS" `eq` str "Stuff")// "catProd"
        `hjoin` (products `indexby` col "WARE") // "prod" `on` col "catProd.WARE"
        
        `hjoin` (materials `indexby` col "BILL_ID") // "mat" `on` col "prod.BILL_ID"
        
        `hjoin` (
                  (categories `wher` col "CLASS" `eq` str "Mineral") 
                  `indexby` col "WARE"
                ) // "catMat" `on` col "mat.WARE"

        `orderby` ["prod.BILL_ID":asc]
        `select` ["prod.BILL_ID", "mat.WARE", "prod.WARE"]
        -- & distinct 
        & limit 0 50 -- skip 0 rows and take 50




-- For the query in the task 2.8 write the plan as efficient as possible.
-- 2.8:
-- Get all the unique companies in alphabetical order implementing production chains. The production
-- chain is at least two subsequent bills of materials when the first bill producing ware that is in use as material
-- in the second bill. Example of such chain in terms of wares is Grain->Meat cow->Meat.
-- Найди компании, которые организовали производственные цепочки (от 2 конвейеров). Например, Grain->Meat cow->Meat

-- SELECT DISTINCT m1.COMPANY
        -- -- ищем компании, которые производят продукты ОДИН
        -- FROM MANUFACTURER m3
        -- -- смотрим на список материалов ТРИ для этого продуктов ТРИ
        -- INNER JOIN MATERIAL mat3 
	      --   ON mat3.BILL_ID == m3.BILL_ID
        -- -- подключаем список чеков, в которых материалы ОДИН производятся
        -- INNER JOIN PRODUCT prod2
	      --   ON prod2.WARE == mat1.WARE
        -- -- узнаём какие компании производят материалы ОДИН / продукты ДВА
        -- -- подключаем только те, что производят продукты ОДИН
        -- INNER JOIN MANUFACTURER m2
	      --   ON m2.COMPANY == m1.COMPANY 
        -- WHERE m2.BILL_ID == prod2.BILL_ID 
	      -- -- узнаём материалы ДВА для продукта ДВА
        -- INNER JOIN MATERIAL mat2
	      --   ON mat2.BILL_ID == m2.BILL_ID

task_5'3 :: (Named Table, Named Table, Named Table, Named Table) -> IO ()
task_5'3 (categories, manufacturers, materials, products) = do
    test "task 5.3" myTask
  
    where
      test msg p = do
        putStrLn $ "===== execute " ++ msg ++ " ====="
        -- putStrLn . debugTable $ p & enumerate
        printResult $ p & enumerate
    

      myTask = 
        -- ORDER BY COMPANY 
        flatten 
        (
          (
            flatten
            (
              (
                flatten (products `indexby` col "BILL_ID") // "prod2"
                `mjoin` flatten (manufacturers `indexby` col "BILL_ID") // "m2" `on` "prod2.BILL_ID" `jeq` "m2.BILL_ID"
              )
              `indexby` col "m2.COMPANY"
            ) 
            `hjoin` (manufacturers `indexby` col "COMPANY") // "m3" `on` col "m2.COMPANY"
            
            `hjoin` (materials `indexby` col "BILL_ID") // "mat3" `on` col "m3.BILL_ID"


            `wher` col "prod2.WARE" `eq` col "mat3.WARE"
          ) 
          
          `indexby` col "m3.COMPANY"
        )
        `select` ["m3.COMPANY"]
        & distinct

      myTaskOrderBy = 
          (
            flatten
            (
              (
                flatten (products `indexby` col "BILL_ID") // "prod2"
                `mjoin` flatten (manufacturers `indexby` col "BILL_ID") // "m2" `on` "prod2.BILL_ID" `jeq` "m2.BILL_ID"
              )
              `indexby` col "m2.COMPANY"
            ) 
            `hjoin` (manufacturers `indexby` col "COMPANY") // "m3" `on` col "m2.COMPANY"
            
            `hjoin` (materials `indexby` col "BILL_ID") // "mat3" `on` col "m3.BILL_ID"


            `wher` col "prod2.WARE" `eq` col "mat3.WARE"
          ) 
        `orderby` ["m3.COMPANY":asc]
        `select` ["m3.COMPANY"]
        & distinct

      myTask0 = 
        flatten
        (
          (
            -- "Завод 3"
            -- (manufacturers `orderby` ["BILL_ID":asc]) // "m3"
            manufacturers // "m3" 
            -- + материал 3 (правый) (он же продукт 2)
            `hjoin` (materials `indexby` col "BILL_ID") // "mat3" `on` col "m3.BILL_ID"
            -- + компания 2 (в середине цепочки), такая же как правая компания (конец цепочки)
            `hjoin` (manufacturers `indexby` col "COMPANY") // "m2" `on` col "m3.COMPANY"
            -- + продукт 2 (он же материал 3, но теперь уже в виде продукта)
            `hjoin` (products `indexby` col "WARE") // "prod2" `on` col "mat3.WARE"
            -- только те продукты 2, которые производит нужная компания
            `wher` col "m2.BILL_ID" `eq` col "prod2.BILL_ID"
          )
          `indexby` col "m3.COMPANY"
        )
        
        `select` ["m3.COMPANY"]
        & distinct

      myTask0OrderBy = 
        -- "Завод 3"
        -- (manufacturers `orderby` ["BILL_ID":asc]) // "m3"
        manufacturers // "m3" 
        -- + материал 3 (правый) (он же продукт 2)
        `hjoin` (materials `indexby` col "BILL_ID") // "mat3" `on` col "m3.BILL_ID"
        -- + компания 2 (в середине цепочки), такая же как правая компания (конец цепочки)
        `hjoin` (manufacturers `indexby` col "COMPANY") // "m2" `on` col "m3.COMPANY"
        -- + продукт 2 (он же материал 3, но теперь уже в виде продукта)
        `hjoin` (products `indexby` col "WARE") // "prod2" `on` col "mat3.WARE"
        -- только те продукты 2, которые производит нужная компания
        `wher` col "m2.BILL_ID" `eq` col "prod2.BILL_ID"

        -- ORDER BY COMPANY 
        `orderby` ["m3.COMPANY":asc]
        `select` ["m3.COMPANY"]

        -- DISTINCT
        & distinct
        
      myTask1 = 
        -- materials // "mat3"
        -- `hjoin` (products `indexby` col "WARE") // "prod2" `on` col "mat3.WARE"
        products // "prod2"
        `hjoin` (materials `indexby` col "WARE") // "mat3" `on` col "prod2.WARE"
        
        `hjoin` (manufacturers `indexby` col "BILL_ID") // "m3" `on` col "mat3.BILL_ID"
        `hjoin` (manufacturers `indexby` col "BILL_ID") // "m2" `on` col "prod2.BILL_ID"
        -- `hjoin` (manufacturers `indexby` col "COMPANY") // "m2" `on` col "m3.COMPANY"

        `wher` col "m2.COMPANY" `eq` col "m3.COMPANY"
        -- `wher` col "m2.BILL_ID" `eq` col "prod2.BILL_ID"

        -- ORDER BY COMPANY 
        `orderby` ["m3.COMPANY":asc]
        `select` ["m3.COMPANY"]

        -- DISTINCT
        & distinct
      
      myTask2 = 
        flatten (products `indexby` col "BILL_ID") // "prod2" 
        `mjoin` flatten (manufacturers `indexby` col "BILL_ID") // "m2" `on` "prod2.BILL_ID" `jeq` "m2.BILL_ID"

        `hjoin` (materials `indexby` col "WARE") // "mat3" `on` col "prod2.WARE"
        `hjoin` (manufacturers `indexby` col "BILL_ID") // "m3" `on` col "mat3.BILL_ID"

        `wher` col "m2.COMPANY" `eq` col "m3.COMPANY"

        -- ORDER BY COMPANY 
        -- `orderby` ["m3.COMPANY":asc]
        `select` ["m3.COMPANY"]

        -- DISTINCT
        & distinct

      myTask3 = 
        flatten (materials `indexby` col "WARE") // "mat3"
        `mjoin` flatten (products `indexby` col "WARE") // "prod2" `on` "mat3.WARE" `jeq` "prod2.WARE"

        `hjoin` (manufacturers `indexby` col "BILL_ID") // "m3" `on` col  "mat3.BILL_ID"
        `hjoin` (manufacturers `indexby` col "BILL_ID") // "m2" `on` col "prod2.BILL_ID"

        `wher` col "m2.COMPANY" `eq` col "m3.COMPANY"

        -- ORDER BY COMPANY 
        `orderby` ["m3.COMPANY":asc]
        `select` ["m3.COMPANY"]

        -- DISTINCT
        & distinct
