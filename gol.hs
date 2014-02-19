{-# LANGUAGE FlexibleInstances #-}
import Control.Comonad
import Control.Monad

class Shiftable m where
  left :: m a -> m a
  right :: m a -> m a
  center :: m a -> a
  around :: Int -> m a -> [a]
  around n x = (reverse . ext $ iterate right x)
               ++ [center x]
               ++ (ext $ iterate left x)
    where ext = map center . take n . tail

data U x = U [x] x [x]
data Life x = Life [U x] (U x) [U x]

instance Shiftable U where
  left (U a b (c:cs)) = U (b:a) c cs
  right (U (a:as) b c) = U as a (b:c)
  center (U _ b _) = b

instance Shiftable Life where
  left (Life a b (c:cs)) = Life (b:a) c cs
  right (Life (a:as) b c) = Life as a (b:c)
  center (Life _ b _) = center b

instance Show (U Bool) where
  show = flip (++) "\n" . map printc . around radius
    where
      printc :: Bool -> Char
      printc True  = '#'
      printc False = '.'

instance Show (Life Bool) where
  show (Life a b c) = "\n" ++
                   ((show =<<) . reverse . take radius $ a) ++
                   show b ++
                   ((show =<<) . take radius $ c)

instance Functor U where
  fmap f (U a b c) = U (map f a) (f b) (map f c)

instance Functor Life where
  fmap f (Life a b c) = Life (map (fmap f) a) (fmap f b) (map (fmap f) c)

liftLife :: (U a -> U b) -> Life a -> Life b
liftLife f (Life a b c) = Life (map f a) (f b) (map f c)

instance Comonad Life where
  duplicate a = Life (tail $ iterate (fmap right) mid) mid (tail $ iterate (fmap left) mid)
    where mid = U (tail $ iterate (liftLife right) a) a (tail $ iterate (liftLife left) a)
  extract = center

rule (Life ((U (a:_) b (c:_)):_) (U (d:_) e (f:_)) ((U (g:_) h (i:_)):_)) =
  (e && nc == 3) ||
  (e && nc == 4) ||
  (not e && nc == 3)
  where n = [a,b,c,d,e,f,g,h,i]
        m = map (\x -> if x then 1 else 0) n
        nc = sum m

------------------------------------------------------------
radius = 5

-- TODO: some nice functions 'fromList' -> U/Life
x = U (repeat False) True (repeat False)
x' = U (repeat False) False (repeat False)
y = Life (repeat x') x (repeat x')

g1 = U (repeat False) True (repeat False)
g2 = right g1
g3 = U (True:(repeat False)) True (True:(repeat False))
glid = Life (g1:(repeat x')) g2 (g3:(repeat x'))

-- .#.
-- ..#
-- ###
