import Control.Comonad
import Data.Monoid

data Iterator a = a :< (Iterator a) deriving Eq
infixr 5 :<

exampleHistory :: Iterator String
exampleHistory =
  "^D"
  :< "^C"
  :< "ls"
  :< "cd"
  :< "foo"
  :< mempty -- from the Monoid instance below

instance Functor Iterator where
  fmap f (a :< it) = (f a) :< (fmap f it)

instance Comonad Iterator where
  extract (a :< _) = a
  duplicate it = tails it
    where tails i@(a :< it) = i :< (tails it)
  -- we can define extend as just `fmap f . duplicate' but here's a
  -- more efficient implementation.
  extend f it@(_ :< as) = (f it) :< (extend f as)

next :: Iterator a -> a
next (_ :< (a :< _)) = a

shift :: Iterator a -> Iterator a
shift (_ :< it) = it

-- now `exampleHistory =>> next' == `shift exampleHistory'.
-- neat, we've changed an "extractor" into a "shifter".

---------------------------------------------------------------------
-- Note that this really doesn't make sense for an infinite iterator.
-- It's here just to make the Show instance below "work".
instance (Monoid a, Eq a) => Monoid (Iterator a) where
  mempty = mempty :< mempty
  a `mappend` b = concat a b
    where concat (a :< it) b | a == mempty = b
                             | otherwise = a :< (concat it b)

-- A bit hackish but sufficient for the demonstration
instance (Monoid a, Eq a, Show a) => Show (Iterator a) where
  show (a :< it@(h :< _)) | h == mempty = show a
                          | otherwise = show a ++ "," ++ show it
