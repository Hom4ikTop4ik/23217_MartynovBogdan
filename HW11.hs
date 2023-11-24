--
import System.IO (hFlush, stdout, hSetBuffering, BufferMode (NoBuffering))
import System.Environment (getArgs)
import Data.List (elemIndex)
import Data.Maybe (catMaybes)

alphabet :: [Char]
alphabet = ['a'..'z']++['A'..'Z']++['0'..'9']

main :: IO ()
main = do
    [mode', key, file_path] <- getArgs
    text <- readFile file_path

    putStr ("mode: \"" ++ mode' ++ "\"\n")
    putStr ("key: \"" ++ key ++ "\"\n")
    putStr ("text:\n\"" ++ text ++ "\"\n\n")

    let mode | mode' == "e" = 1
             | mode' == "d" = (-1)

    let symbol | mode == 1 = "+"
               | mode == (-1) = "-"

    putStr ("text " ++ symbol ++ " key:\n\"" ++ (vigenere text key mode) ++ "\"\n")
    writeFile ("out_"++file_path) (vigenere text key mode)


vigenere :: String -> String -> Int -> String
vigenere text key' mode =
    let key = cycle key' in
    fst (foldl (\(acc, ind) x -> if x `notElem` alphabet
                    then (acc ++ [x], ind) -- if x не из alphabet, то ключ Виженера не сдвигаем
                    else (acc ++ [int_2_char (  62 + char_2_int x + mode*(char_2_int (key !! ind))  )], ind+1)
        ) ("", 0) text)                                       -- ↑ add or subtract key?

char_2_int :: Char -> Int
char_2_int char = unJust (elemIndex char alphabet)
                  where unJust (Just a) = a

int_2_char :: Int -> Char
int_2_char int = (cycle alphabet) !! int
