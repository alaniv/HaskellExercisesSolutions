-- ex1
data Nat = Zero | Succ Nat

add2 :: Nat -> Nat -> Nat
add2 Zero m = m
add2 (Succ n) m = Succ (add2 n m)

mult2 :: Nat -> Nat -> Nat
mult2 Zero m = Zero
mult2 (Succ n) m = add2 (mult2 n m) m

-- ex2
-- data Ordering = LT | EQ | GT
-- compare :: Ord a => a -> a -> Ordering
-- usage of compare reduces comparisons on a's
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs2 :: Ord a => a -> Tree a -> Bool
occurs2 x (Leaf y)                 = x == y
occurs2 x (Node l y r) = case compare x y of EQ -> True
                                             LT -> occurs2 x l
                                             GT -> occurs2 x r

-- ex3
data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a)

countLeaves :: Tree2 a -> Int
countLeaves (Leaf2 x) = 1
countLeaves (Node2 n m) = countLeaves(n) + countLeaves(m)

balanced :: Tree2 a -> Bool
balanced (Leaf2 x) = True
balanced (Node2 l r) | abs(countLeaves(r) - countLeaves(l)) <= 1 = balanced(l) && balanced(r)
                     | otherwise = False

-- ex4
halve :: [a] -> ([a], [a])
halve xs = (take mid xs , drop mid xs)
            where mid = (length xs) `div` 2
            
balance :: [a] -> Tree2 a
balance [x] = Leaf2 x
balance xs = Node2 (balance xs1) (balance xs2)
            where (xs1, xs2) = halve xs

-- ex5

main :: IO ()
main = do
    print("nada")
