-- ex1
{- Duplicate instance
instance (Monoid a, Monoid b) => Monoid (a,b) where
    -- mempty :: (a,b)
    mempty = (mempty, mempty)
    -- mappend :: (a,b) -> (a,b) -> (a,b)
    (x1,y1) `mappend` (x2,y2) = (x1 `mappend` x2, y1 `mappend` y2)
-}

-- ex2
{- Duplicate instance
instance Monoid b => Monoid (a -> b) where
    -- mempty :: a -> b
    mempty _ = mempty
    -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
    (fa `mappend` ga) x = (fa x) `mappend` (ga x)
-}

-- ex3
-- show how Maybe type can be made foldable and traversable
{- Duplicate instance
import Data.Foldable
instance Foldable Maybe where
    -- fold :: Monoid a => Maybe a -> a
    fold Nothing = mempty
    fold (Just xa) = xa
    -- foldMap :: Monoid b => (a -> b) -> Maybe a -> b
    foldMap fa Nothing = mempty
    foldMap fa (Just xa) = fa xa
    -- foldr :: (a -> b -> b) -> b -> Maybe a -> b
    foldr fab xb Nothing = xb
    foldr fab xb (Just xa) = fab xa b
    --foldl :: (a -> b -> a) -> a -> Maybe b -> a
    foldl fab xa Nothing = xa
    foldl fab xa (Just xb) = fab xa xb
    
instance Traversable Maybe where
    --traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
    traverse _ Nothing = pure Nothing
    traverse g (Just xa) = Just (g xa)
-}

-- ex4
import Data.Foldable
data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g Leaf = Leaf
    fmap g (Node tl e tr) = Node (fmap g tl) (g e) (fmap g tr)

instance Foldable Tree where
    -- fold :: Monoid a => Tree a -> a
    fold Leaf = mempty
    fold (Node tl val tr) = (fold tl) `mappend` val `mappend` (fold tr)
    -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
    foldMap fa Leaf = mempty
    foldMap fa (Node tl val tr) = (foldMap fa tl) `mappend` (fa val) `mappend` (foldMap fa tr)
    -- foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr fab xb Leaf = xb
    foldr fab xb (Node tl val tr) = foldr fab (fab val (foldr fab xb tl)) tr

    --foldl :: (a -> b -> a) -> a -> Tree b -> a
    foldl fab xa Leaf = xa
    foldl fab xa (Node tl val tr) = foldl fab (fab (foldl fab xa tl) val) tr
    
instance Traversable Tree where
    --traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse g Leaf = pure Leaf
    traverse g (Node tl val tr) = pure Node <*> (traverse g tl) <*> (g val ) <*> (traverse g tr)
    
-- ex5
filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF pred xfa = foldMap (\x -> if (pred x) then [x] else []) xfa

main :: IO ()
main = do
    print $ filterF (\x -> x == 1) [1,2,3,4]
