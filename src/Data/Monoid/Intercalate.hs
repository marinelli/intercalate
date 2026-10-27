-- | Functions that join 'Semigroup' and 'Monoid' values with a separator
-- between them.
module Data.Monoid.Intercalate (
  interpose,
  interposeSkipEmpty,
  mintercalate,
  mintercalateSkipEmpty,
) where

import Data.Foldable
import Data.List

-- | @interpose s a b@ puts the separator @s@ between @a@ and @b@.
--
-- >>> interpose ", " "left" "right"
-- "left, right"
interpose :: Semigroup s => s -> s -> s -> s
interpose s left right = left <> s <> right

-- | @interposeSkipEmpty s a b@ is like 'interpose', but it puts the
-- separator @s@ only if neither @a@ nor @b@ is 'mempty'. If one of them is
-- 'mempty', the result is the other one.
--
-- >>> interposeSkipEmpty ", " "left" "right"
-- "left, right"
-- >>> interposeSkipEmpty ", " "left" ""
-- "left"
interposeSkipEmpty :: (Eq s, Monoid s) => s -> s -> s -> s
interposeSkipEmpty s left right
  | left == mempty = right
  -- The result starts with left before right is compared with mempty. So on
  -- a lazy type, e.g. String, a right fold over an infinite list streams.
  | otherwise = left <> if right == mempty then mempty else s <> right

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

-- | @mintercalateSkipEmpty s xs@ is like 'mintercalate', but it leaves out
-- the elements of @xs@ that are 'mempty'. So the result never has two
-- separators side by side, or a separator at either end.
--
-- >>> mintercalateSkipEmpty ", " ["a", "", "b", ""]
-- "a, b"
--
-- If a join of two non-empty values is never 'mempty', e.g. on 'String',
-- @Text@ or @ByteString@, it is a right fold of 'interposeSkipEmpty'. On an
-- infinite list whose elements are all 'mempty', it does not terminate.
mintercalateSkipEmpty :: (Foldable f, Eq a, Monoid a) => a -> f a -> a
mintercalateSkipEmpty s = mintercalate s . filter (/= mempty) . toList
