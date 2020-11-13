double :: Int -> Int
double x = x + x

sum2 :: [Int] -> Int
sum2 [] = 0
sum2 (x:xs) = x + sum2(xs)

product2 :: [Int] -> Int
product2 [] = 1
product2 (x:xs) = x * product2(xs)

rqsort :: [Int] -> [Int]
rqsort [] = []
rqsort (x:xs) = rqsort larger ++ [x] ++ rqsort smaller
    where 
        larger = [a | a <- xs, a > x]
        smaller = [a | a <- xs, a <= x]

qsort2 :: [Int] -> [Int]
qsort2 [] = []
qsort2 (x:xs) = qsort2 smaller ++ [x] ++ qsort2 larger
    where 
        larger = [a | a <- xs, a > x]
        smaller = [a | a <- xs, a < x]


main :: IO ()
main = do
    print (double 22)
    print (sum2 [1,2,3,4])
    print (product2 [2,3,4])
    print (rqsort [5,22,4,7,1])
    print (qsort2[2,2,3,1,1]) --duplicated elements were removed...
    
    
