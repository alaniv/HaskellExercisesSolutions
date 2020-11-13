num :: Int
num = a `div` (length xs)
    where
        a = 10
        xs = [1,2,3,4,5]

last2 :: [Int] -> Int
last2 [x] = x
last2 (x:xs) = last xs

init2 :: [Int] -> [Int]
init2 x = reverse (tail (reverse x))

init3 :: [Int] -> [Int]
init3 x = take (length x - 1) x


main :: IO ()
main = do
    print(num)
    print(last2 [1,2,3,4,5])
    print(init2 [1,2,3,4,5])
    print(init3 [1,2,3,4,5])
