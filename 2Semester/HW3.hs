
-- как вариант в State добавить: 
-- 1) глубину, в которой я сейчас нахожусь
-- 2) стэк указателей куда возвращаться


import System.IO (hFlush, stdout, hSetBuffering, BufferMode (NoBuffering))
import System.Environment (getArgs)
import Control.Monad.State
import Data.Char (chr, ord)

-- chr (32) is " "
-- ord (" ") is 32

-- if set 1000+ it will be very long
howMuch = 100

-- array with zeros
data' :: [Int]
data' = [y | x <- [1..howMuch], y <- [0*x]]

-- infinite buffer with '\0'
out' :: String
out' = []


main :: IO ()
main = do
    temp <- getArgs
    program <- readFile $ head temp

    -- in' is second argument
    -- let in' = tail temp
    -- if User forgets write second argument in "", second arg will be separated by spaces
    let in' = foldl (\acc x -> acc ++ x ++ " ") [] (tail temp) :: String

    -- -- Debug:
    -- let temp args = fst $ foldl (\(text, i) x -> (text ++ "\n" ++ show i ++ ": " ++ x, i+1)) ([], 0) args
    -- putStr ("Path: " ++ filePath ++ "\nArgs: " ++ (temp args))

    putStr $ evalState brainFck (program, in', data', out', {-dataPtr-} 0, {-programPtr-} 0, {-depth-}0, {-stack for cycles-}[])

program = ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.>>>++++++++[<++++>-]<.>>>++++++++++[<+++++++++>-]<---.<<<<.+++.------.--------.>>+.>++++++++++."

in' = ""
brainTest = evalState brainFck (program, in', data', out', {-dataPtr-} 0, {-programPtr-} 0, {-depth-}0, {-stack for cycles-}[])

-- program, input buffer, data buffer, out buffer, dataPtr, programPtr, depth, stack for cycles 
brainFck :: State (String, String, [Int], String, Int, Int, Int, [Int]) [Char]
brainFck = do
    (program, in', data', out', dataPtr, programPtr, depth, stack) <- get

    put $ brainHelper (program, in', data', out', dataPtr, programPtr, depth, stack)

    if programPtr == (length program)
        then return out'
        else do brainFck

brainHelper :: (String, String, [Int], String, Int,     Int,        Int,   [Int]) -> (String, String, [Int], String, Int, Int, Int, [Int])
brainHelper   (program, in',    data', out',   dataPtr, programPtr, depth, stack) 
    | command == '>' = (program, in', data', out', dataPtr+1, programPtr+1, depth, stack) -- dataPtr++
    | command == '<' = (program, in', data', out', dataPtr-1, programPtr+1, depth, stack) -- dataPtr--
    | command == '+' = (program, in', updData data' dataPtr ((data' !! dataPtr) + 1), out', dataPtr, programPtr+1, depth, stack) -- (*dataPtr)++
    | command == '-' = (program, in', updData data' dataPtr ((data' !! dataPtr) - 1), out', dataPtr, programPtr+1, depth, stack) -- (*dataPtr)--
    | command == '.' = (program, in', data', out' ++ [chr (data' !! dataPtr)], dataPtr, programPtr+1, depth, stack) -- print(*dataPtr)
    | command == ',' = (program, tail in', updData data' dataPtr (ord $ head in'), out', dataPtr, programPtr+1, depth, stack) -- scanf("%c",*dataPtr)
    | command == '[' = if data' !! dataPtr == 0
                        then -- skip cycle
                            (program, in', data', out', dataPtr, getCloseBracketIndex program programPtr, depth, stack)
                        else -- start cycle 
                            (program, in', data', out', dataPtr, programPtr+1, depth+1, programPtr : stack) 
    | command == ']' = if data' !! dataPtr == 0
                        then -- break;
                            (program, in', data', out', dataPtr, programPtr+1, depth-1, stack)
                        else -- next iteration 
                            (program, in', data', out', dataPtr, head stack, depth, tail stack)
    where command = program !! programPtr

updData :: [Int] -> Int -> Int -> [Int]
updData data' index upd = fst $ foldl (\(acc, i) x -> (if i == index then acc ++ [upd] else acc ++ [x], i+1)) ([], 0) data'

-- intMod x = (x + 2147483648) `mod` 4294967295 - 2147483648

getCloseBracketIndex :: [Char] -> Int -> Int
getCloseBracketIndex program start = -- start is '[' index (start our cycle)
    head (foldl func [(-1), 0, 0, 0] program)
        where func [ans, depth, i, flag] x | i < start = [ans, depth,   i+1, flag] -- skip
                                           | flag == 1 = [ans, depth,   i+1, flag] -- "return" 
                                           | x == '['  = [ans, depth+1, i+1, flag]
                                           | x == ']'  = [ans, depth-1, i+1, flag]
                                           | depth == 0= [i,   depth,   i+1, 1   ]
                                           | otherwise = [ans, depth,   i+1, flag]

--  head (foldl (\[ans, depth, i, flag] x -> 
--         if (i < start) || (flag == 1) then [ans, depth, i+1, flag] -- do nothing
--         else
--             if x == '[' then [ans, depth+1, i+1, flag] -- open cycle
--             else if x == ']' then [ans, depth-1, i+1, flag] -- close cycle
--             else if depth == 0 
--                 then [i, depth, i+1, 1] -- end our cycle
--                 else [ans, depth, i+1, flag] -- next cell
--     ) [(-1), 0, 0, 0] program)
