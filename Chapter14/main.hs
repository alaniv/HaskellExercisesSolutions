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
