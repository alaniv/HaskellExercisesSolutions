main :: IO ()
main = do
    print( head [1,2,3,4,5] )
    print( tail [1,2,3,4,5] )
    print( take 3 [1,2,3,4,5] )
    print( drop 3 [1,2,3,4,5] )
    print( length [1,2,3,4,5] )
    print( sum [1,2,3,4,5] )
    print( product [1,2,3,4,5] )
    print( [1,2,3] ++ [4,5] )
    print( reverse [1,2,3,4,5] )
    
{-
1
[2,3,4,5]
[1,2,3]
[4,5]
5
15
120
[1,2,3,4,5]
[5,4,3,2,1]
-}
