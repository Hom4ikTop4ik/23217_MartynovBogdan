--da
import Data.Maybe

data Person = Person {
    name    :: String,
    surname :: String,
    mother  :: Maybe Person,
    father  :: Maybe Person
} deriving Show

-- instance Monad Maybe where -- Monad Maybe has this by default
--     return :: a -> Maybe a
--     return x = Just x

--     (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--     Nothing  >>= f = Nothing
--     (Just x) >>= f = f x

-- tests
{--
Jane    Russ    Bruh    Mira
  \      /        \      /
    Alice           John
      \              /
            David
--}  

-- task 1
jane    = Person "Jane"     "Smith"     Nothing Nothing
russ    = Person "Russ"     "Cox"       Nothing Nothing
miranda = Person "Miranda"  "Lee"       Nothing Nothing
bruh    = Person "Bruh"     "Peterson"  Nothing Nothing
alice   = Person "Alice"    "Cox"       (Just russ) (Just jane)
john    = Person "John"     "Lee"       (Just bruh) (Just miranda)
david   = Person "David"    "Lee"       (Just john) (Just alice)

motFatPattern :: Person -> Maybe Person
motFatPattern p = case mother p of -- mama
    Nothing -> Nothing
    Just m -> case father m of -- ded by mama
        Nothing -> Nothing
        Just ded -> Just ded

motFatMonad :: Person -> Maybe Person
motFatMonad p = mother p >>= (\mama -> father mama)



-- task 2
hasAllGrands :: Person -> Maybe Person
hasAllGrands p = 
      mother p >>= (\mama -> mother mama)  >> 
        mother p >>= (\mama -> father mama)  >>
          father p >>= (\papa -> mother papa)  >> 
            father p >>= (\papa -> father papa)



-- task 3
sumTwoInts :: IO ()
-- sumTwoInts = do 
--         print "Vvedite 2 chisla cherez Enter:"
--         x <- readLn :: IO Int
--         y <- readLn :: IO Int
--         print (x + y)
sumTwoInts = (readLn :: IO Int) >>= (\x ->
              (readLn :: IO Int) >>= (\y ->
                print (x+y) ) )
-- sumTwoInts = readInt >>= (\x ->
--               readInt >>= (\y ->
--                 print (x+y) ) )
--                   where readInt = readLn :: IO Int
