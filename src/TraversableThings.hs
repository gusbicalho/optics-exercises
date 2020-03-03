module TraversableThings where

import Data.Traversable

data Foo a
  = Bar a a a
  | Qux a [a]
  deriving (Eq, Show)

instance Functor Foo where
  fmap f (Bar a b c) = Bar (f a) (f b) (f c)
  fmap f (Qux a bs) = Qux (f a) (f <$> bs)

instance Foldable Foo where
  foldMap f (Bar a b c) = f a <> f b <> f c
  foldMap f (Qux a bs) = f a <> foldMap f bs

instance Traversable Foo where
  traverse f (Bar a b c) = Bar <$> f a <*> f b <*> f c
  traverse f (Qux a bs) = Qux <$> f a <*> traverse f bs

  sequenceA (Bar fa fb fc) = Bar <$> fa <*> fb <*> fc
  sequenceA (Qux fa fbs) = Qux <$> fa <*> sequenceA fbs
