-- Nim
import System.IO
import Data.Char

nextPlayer :: Int -> Int
nextPlayer 1 = 2
nextPlayer 2 = 1

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
    where update r n = if r == row then (n - num) else n

--
putRow :: Int -> Int -> IO()
putRow row num = do 
    putStr (show row)
    putStr ": "
    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO()
putBoard [a,b,c,d,e] = do
    putRow 1 a
    putRow 2 b
    putRow 3 c
    putRow 4 d
    putRow 5 e

getDigit :: String -> IO Int
getDigit prompt = do 
    putStr prompt
    x <- getChar
    putChar '\n'
    if isDigit x then
        return (digitToInt x)
    else
        do 
        putStrLn "Error: Invalid digit"
        getDigit prompt
--
play :: Board -> Int -> IO()
play board player = do
    putChar '\n'
    putBoard board
    if finished board then
        do 
        putChar '\n'
        putStr "Player "
        putStr (show (nextPlayer player))
        putStrLn " wins!"
    else
        do 
        putChar '\n'
        putStr "Player "
        putStr (show player)
        row <- getDigit "Enter a row number: "
        num <- getDigit "Stars to remove: "
        if valid board row num then
            play (move board row num) (nextPlayer player)
        else
            do 
            putChar '\n'
            putStrLn "Error: Invalid move"
            play board player

main :: IO()
main = play initial 1
