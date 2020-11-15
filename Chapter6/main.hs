-- ex 1
fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n-1)

-- ex 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- ex 3
exp2 :: Int -> Int -> Int
exp2 n 0 = n
exp2 n m | m > 0 =  n * (exp2 n (m-1) )

-- ex 4
euclid :: Int -> Int -> Int
euclid x 0 = x
euclid x y | x < y = euclid y x
euclid x y | x >= y = euclid y (mod x y)

-- ex 6
and2 :: [Bool] -> Bool
and2 [] = True
and2 (x:xs) | x == True = and2(xs)
            | x == False = False

concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (x:xs) = x ++ concat(xs)

replicate2 :: Int -> a -> [a]
replicate2 0 e = []
replicate2 n e | n > 0 = [e] ++ replicate2 (n-1) e

selNth :: [a] -> Int -> a
selNth (x:xs) 0 = x
selNth (_:xs) n | n > 0 = selNth xs (n-1)

elem2 :: Eq a => a -> [a] -> Bool
elem2 e [] = False
elem2 e (x:xs) | e == x = True
               | otherwise = elem2 e xs

-- ex 7
merge2 :: Ord a => [a] -> [a] -> [a]
merge2 xs [] = xs
merge2 [] ys = ys
merge2 (x:xs) (y:ys) | x < y = [x] ++ merge2 xs (y:ys)
                     | otherwise = [y] ++ merge2 (x:xs) ys
                     
-- ex 8
halve :: [a] -> ([a], [a])
halve xs = (take mid xs , drop mid xs)
            where mid = (length xs) `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:[]) = [x]
msort x = merge2 (msort (fst h)) (msort (snd h))
            where h = halve x

main :: IO ()
main = do
    print(fac 2)
    print(sumdown 3)
    print(2 `exp2` 3)
    print(euclid 15 21)
    print (and2 [True, True, True])
    print (concat2 [[1,2,3], [4,5,6]])
    print (replicate2 5 'a')
    print( selNth [1,2,3,4] 2)
    print(elem2 'a' "berebe")
    print(merge2 [1,4,8] [2,5,9])
    print(msort [9,8,7,1,2,3,0])
