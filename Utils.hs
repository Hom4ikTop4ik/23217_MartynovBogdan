module Utils (
    split, strIsNumber, strToInt
) where

import Data.Char

-- ghci> split "Hello world !!!" '\32'
-- ["Hello", "world", "!!!"]
split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
    | x == delim = "" : rest
    | otherwise = (x : head rest) : tail rest
    where
        rest = split xs delim

strIsNumber :: String -> Bool
strIsNumber = all isDigit

strToInt :: String -> Int
strToInt s  | strIsNumber s = read s
            | otherwise = error "This string is not a number"