sumSquares :: Int
sumSquares = sum [ x^2 | x <- [1..100]]

--ex 2
grid :: Int -> Int -> [(Int, Int)]
grid n m = [(x,y) | x <- [0..n], y <- [0..m]]

--ex3
square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

--ex4
replicate2 :: Int -> a -> [a]
replicate2 n e = [e | _ <- [1..n]]

--ex5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- ex 6
divisors :: Int -> [Int]
divisors n = [ x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n] , sum (divisors x) == 2*x ]

--ex7
pairs :: [(Int,Int)]
pairs = concat [ [(x,y) | y <- [3,4] ] | x <- [1,2] ]

--ex8
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [ v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [ i |  (x', i) <- zip xs [0..], x == x']

positions2 :: Eq a => a -> [a] -> [Int]
positions2 x xs = find x (zip xs [0..])

--ex9
scalarProd :: [Int] -> [Int] -> Int
scalarProd xs ys = sum [ x*y | (x,y) <- zip xs ys ]

main :: IO ()
main = do
    print(sumSquares)
    print(grid 3 5)
    print(square 2)
    print(replicate2 3 True)
    print(pyths 10)
    print(perfects 496)
    print(pairs)
    print(positions2 1 [1,2,3,4,1,2,3,4])
    print(scalarProd [1,2,3] [4,5,6])
