module Chapter6.OneMore where

import Data.Foldable

data OneMore f a = OneMore a (f a)

instance showOneMore :: (Show a, Show (f a)) => Show (OneMore f a) where
    show (OneMore a b) = show a ++ ":" ++ show b

instance foldableOneMore :: (Foldable f) => Foldable (OneMore f) where
    foldr f z (OneMore a as) = f a (foldr f z as)
    foldl f z (OneMore a as) = f (foldl f z as) a
    foldMap f (OneMore a as) = f a <> foldMap f as