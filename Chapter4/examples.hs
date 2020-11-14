-- new from old  ?
splitAtt :: Int -> [a] -> ([a],[a])
splitAtt n xs = (take n xs, drop n xs)

--conditional
abss :: Int -> Int
abss n = if n >= 0 then n else -n

--guarded
abs2 :: Int -> Int
abs2 n 
    | n >= 0 = n
    | otherwise = -n

--patttern matching
(%%) :: Bool -> Bool -> Bool
True %% True = True
_ %% _ = False

--lambda expressions
odds2 :: Int -> [Int]
odds2 n = map (\x -> x*2 + 1) [0..n-1]

--operator section
minus2 :: [Int] -> [Int]
minus2 xs =  map (2 - ) xs

main :: IO()
main = do
    print (splitAtt 2 [1,2,3,4,5])
    print ( abss (-5) )
    print ( abs2 (-5) )
    print (True %% False)
    print (odds2 7)
    print( minus2 [4,5,6])
