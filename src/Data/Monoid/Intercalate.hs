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
--
-- On a strict @ByteString@, use @Data.ByteString.intercalate@ if speed
-- matters. It copies the pieces straight into one buffer, but
-- 'mintercalate' first builds a list of the pieces and the separators.
-- With 100,000 short pieces, @Data.ByteString.intercalate@ was about 5
-- times faster, and it allocated about 12 times less memory. These numbers
-- come from the ByteString group of the benchmark of this package, run on
-- its own with GHC 9.12.4 and bytestring-0.12.2.0. The speed ratio depends
-- on the state of the heap: after the other groups, it was about 8.
mintercalate :: (Foldable f, Monoid a) => a -> f a -> a
mintercalate s = mconcat . intersperse s . toList
