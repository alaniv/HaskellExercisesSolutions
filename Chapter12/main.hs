-- ex1
data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving Show
    
instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g Leaf = Leaf
    fmap g (Node tl e tr) = Node (fmap g tl) (g e) (fmap g tr)

-- ex2
{- duplicated instance declaration in GHC.Base
instance Functor ((->) c) where
    -- fmap :: (a -> b) -> (c -> a) -> (c -> b)
    -- signature is the same that compose operator
    fmap = (.)
-}

-- ex3
-- applicative for ((->) c)
{- duplicated instance declaration in GHC.Base
instance Applicative ((->) c) where
    --pure :: a -> (c-> a)
    pure e = (\x -> e) -- i.e. pure = const
    -- (<*>) :: (c -> (a -> b)) -> (c -> a) -> (c -> b)
    g <*> h xc = g xc (h xc)
    -- fullfills the 4 applicative laws.
--}

-- ex4
newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    --fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (Z xs) = Z (fmap g xs)
    
instance Applicative ZipList where
    -- pure :: a -> ZipList a
    pure x = Z (fmap (const x) [1..] ) -- repeat x
    
    -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
    (Z gs) <*> (Z xs) = Z [ g x | (g,x) <- zip gs xs]

-- ex5
{- duplicated Defined in GHC.Base
instance Monad ((->) c) where
    -- (>>=) :: (c -> a) -> (a -> (c -> b)) -> (c -> b)
    (fca >>= g) xc = g (fca xc) xc
-}

main :: IO ()
main = do
    print $ fmap (+1) (Node Leaf 22 Leaf)
    print $ Z [1,2,3]
    -- print $ (pure 'a' :: ZipList Char)
