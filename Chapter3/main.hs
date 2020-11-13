-- ex 2
bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[1,2], [3,4]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a,a)
copy x = (x,x)

apply :: (a->b) -> a -> b
apply f x = (f x)

-- ex 3
second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

ispalindrome :: Eq a => [a] -> Bool
ispalindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)


main :: IO ()
main = do
    print(apply reverse [1,2,3])
