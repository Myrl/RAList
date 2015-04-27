import Data.Function
import Control.Applicative
import Data.Functor
import Data.Foldable

data STree a = Node a (STree a) (STree a) | Leaf a
data SList a = SList Int (STree a) (SList a) | SNothing

instance Functor STree where
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)
  fmap f (Leaf x)     = Leaf (f x)

instance Functor SList where
  fmap _ SNothing = SNothing
  fmap f (SList i x xs) = SList i (fmap f x) (fmap f xs)

instance Applicative SList where
  pure x = SList 1 (Leaf x) SNothing
  fs <*> xs  = sfromlist $ (stolist fs) <*> (stolist xs)
  _ *> x = x
  x <* _ = x

instance Monoid (SList a) where
  mempty = SNothing
  mappend SNothing ys = ys
  mappend xs ys = scons (shead xs) (mappend (stail xs) ys)

sfoldl :: (b -> a -> b) -> b -> SList a -> b
sfoldl f x SNothing = x
sfoldl f x (SList i y ys) = sfoldl f (sfoldl' f x y) ys
  where
    sfoldl' f x (Node y l r) = sfoldl' f (sfoldl' f (f x y) l) r
    sfoldl' f x (Leaf y) = f x y

shead :: SList a -> a
shead SNothing = error "empty"
shead (SList _ (Node x _ _) _) = x
shead (SList _ (Leaf x) _) = x

stail :: SList a -> SList a
stail SNothing = error "empty"
stail (SList 1 _ xs) = xs
stail (SList i (Node _ x x') xs) = SList i' x $ SList i' x' xs
  where i' = div i 2

scons :: a -> SList a -> SList a
scons x (SList i y (SList i' y' (ys)))
  | i == i' = SList (1 + 2*i) (Node x y y') ys
scons x ys = SList 1 (Leaf x) ys

sfromlist :: [a] -> SList a
sfromlist [] = SNothing
sfromlist (x:xs) = scons x $ sfromlist xs

stolist :: SList a -> [a]
stolist SNothing = []
stolist x = shead x : stolist (stail x)

sindex :: Int -> SList a -> a
sindex i SNothing = error "overflow"
sindex i (SList i' x xs)
  | i >= i' = sindex (i - i') xs
  | otherwise = find i i' x
      where
        find i s (Node x l r)
          | i == 0 = x
          | i <= s' = find (i - 1) s' l
          | i > s' = find (i - s' - 1) s' r
          where s' = div s 2
        find _ _ (Leaf x) = x

slength :: SList a -> Int
slength (SList i _ xs) = i + (slength xs)

stake :: Int -> SList a -> SList a
stake 0 _ = SNothing
stake _ SNothing = SNothing
stake n x = scons (shead x) (stake (n - 1) (stail x))

main = print $ sfoldl (+) 0 $ fmap succ $ sfromlist [1..10]
