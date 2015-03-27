module Chapter6.Actions where

import Data.Monoid

class (Monoid m) <= Action m a where
    act :: m -> a -> a

foreign import undefined :: forall a . a

instance addNumbers :: Semigroup Number where
    (<>) = (+)

instance addNumbersMonoid :: Monoid Number where
    mempty = 0

instance repeatAction :: Action Number String where
    act 0 _ = ""
    act n s = s ++ act (n - 1) s

instance arrayAction :: (Monoid m, Action m a) => Action m [a] where
    act m xs = act m <$> xs
