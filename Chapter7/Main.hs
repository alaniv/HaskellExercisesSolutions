-- ex1
fun1 :: [a] -> (a -> b) -> (a -> Bool) -> [b]
fun1 xs f p = map f (filter p xs)

-- ex2
alla :: (a -> Bool) -> [a] -> Bool
alla p xs = length xs == length (filter (== True) (map p xs))

anyb :: (a -> Bool) -> [a] -> Bool
anyb p xs = length (filter (== True) (map p xs)) > 0

takewhilec :: (a->Bool) -> [a] -> [a]
takewhilec p [] = []
takewhilec p (x:xs) | (p x) = [x] ++ (takewhilec p xs)
                    | otherwise = []

dropwhiled :: (a->Bool) -> [a] -> [a]
dropwhiled p [] = []
dropwhiled p (x:xs) | (p x) = (dropwhiled p xs)
                    | otherwise = xs

-- ex3
map2 :: (a -> b) -> [a] -> [b]
map2 f xs = foldr ( \y ys -> (f y):ys) [] xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p xs = foldr ( \y ys -> if (p y) then y:ys else ys) [] xs

-- ex4
dec2int :: [Int] -> Int
dec2int xs = foldl ( \x y -> x*10 + y) 0 xs

-- ex5
curry2 :: ((a,b) -> c) -> (a -> b -> c)
curry2 f = \x -> (\y -> f (x,y) )

uncurry2 :: (a -> b -> c) -> ((a,b) -> c)
uncurry2 f (x,y) = f x y 

-- ex9
altMap :: (a->b) -> (a->b) -> [a] -> [b]
altMap f1 f2 [] = []
altMap f1 f2 (x:xs) = [f1 x] ++ altMap f2 f1 xs

main :: IO()
main = do
    print(fun1 [1,2,3] (*2) (/=2))
    print(alla id [True, True])
    print(anyb id [False, False])
    print(takewhilec id [True, True, False, True])
    print(dropwhiled id [True, True, False, True])
    print(map2 (\x -> x*2) [1,2,3,4])
    print(filter2 id [True, False, True, False, False])
    print(dec2int [1,2,3,4])
    print(altMap (\x -> x*2) (\x -> x*3) [1,2,3,4,5,6])
