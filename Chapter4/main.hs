-- 1
halve :: [a] -> ([a], [a])
halve xs = (take mid xs , drop mid xs)
            where mid = (length xs) `div` 2

--2
third1 :: [a] -> a
third1 xs = head( tail (tail xs))

third2 :: [a] -> a
third2 xs = xs !! 2

third3 :: [a] -> a
third3 (x:y:z:xs) = z

--3
safetail1 :: [a] -> [a]
safetail1 xs = if null xs then xs else tail xs

safetail2 :: [a] -> [a]
safetail2 xs
    | null xs = xs
    | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 xs = tail xs

--5
and5 :: Bool -> Bool -> Bool
and5 x y = if x then (if y then True else False) else False

--6
and6 :: Bool -> Bool -> Bool
and6 x y = if x then y else False

--7 lambda expression
mult7 :: Int -> Int -> Int -> Int
mult7 = \x -> \y -> \z -> x*y*z

--8
luhnDouble :: Int -> Int
luhnDouble n = n*2 - if (n > 4) then 9 else 0

luhn :: Int -> Int -> Int -> Int -> Bool
luhn n1 n2 n3 n4 = luhnSum `mod` 10 == 0
    where luhnSum = (luhnDouble n1) + n2 + (luhnDouble n3) + n4

main :: IO()
main = do
    print (halve [1,2,3,4,5,6])
    print (third1 [111,222,333])
    print (third2 [111,222,333])
    print (third3 [111,222,333])
     --explicit type for ambiguity resolution...
    print (safetail1 [] :: [Int])
    print (safetail2 [] :: [Int]) 
    print (safetail3 [] :: [Int]) 
    print (True `and5` False)
    print (True `and6` False)
    print(mult7 3 4 2)
    print (luhn 1 7 8 4)
    print (luhn 4 7 8 3)
