-- | Functions that join 'Semigroup' and 'Monoid' values with a separator
-- between them.
module Data.Monoid.Intercalate (
  interpose,
  mintercalate,
) where

import Data.Foldable
import Data.List

-- | @interpose s a b@ puts the separator @s@ between @a@ and @b@.
--
-- >>> interpose ", " "left" "right"
-- "left, right"
interpose :: Semigroup s => s -> s -> s -> s
interpose s left right = left <> s <> right

-- | @mintercalate s xs@ puts the separator @s@ between each pair of
-- neighboring elements of @xs@, and concatenates the result. It is
-- 'Data.List.intercalate' for any 'Foldable' of 'Monoid' values. If @xs@
-- is empty, the result is 'mempty'.
--
-- >>> mintercalate ", " ["a", "b", "c"]
-- "a, b, c"
mintercalate :: (Foldable f, Monoid a) => a -> f a -> a
mintercalate s = mconcat . intersperse s . toList
