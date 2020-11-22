import System.IO

-- ex1
-- sequence_ :: [IO a] -> IO ().
putStr2 :: String -> IO ()
putStr2 xs = sequence_ [ putChar x | x <- xs]

-- ex4
adderAux :: Int -> Int -> IO ()
adderAux 0 t = putStrLn $ show t
adderAux n t = do
    x <- getLine
    adderAux (n-1) (t + (read x :: Int))

adder :: IO ()
adder = do
    x <- getLine
    adderAux (read x :: Int) 0
    
-- ex5
-- sequence  ::  [IO  a]  ->  IO  [a]
adder2 :: IO ()
adder2 = do
    n <- getLine
    nums <- sequence [getLine | _ <- [1..(read n :: Int)]]
    putStrLn $ show $ foldr (+) 0 [(read x :: Int) | x <- nums]

-- ex6
readLine2 :: IO String
readLine2 = do
    x <- getChar
    if x == '\n' then
        return []
    else if x == '\DEL' then
        do
        xs <- readLine2
        return ('\b':xs)
    else 
        do 
        xs <- readLine2
        return (x:xs)
    


main = do
    hSetBuffering stdout NoBuffering
    putStr2 "asf\n"
    putStrLn "adder 1"
    adder
    putStrLn "adder 2"
    adder2
    putStrLn "readLine2"
    x <- readLine2
    putStrLn x
