-- | Variants of the functions in "Data.Monoid.Intercalate" that leave out
-- empty values. They never put a separator next to an empty value. The
-- plain variants find the empty values with 'Eq'. The variants that end
-- in @By@ take a predicate instead, for a type without an 'Eq' instance.
--
-- "Data.Intercalate.SkipEmpty" has the same functions with faster joins
-- for the types of @text@ and @bytestring@.
module Data.Monoid.Intercalate.SkipEmpty (
  interposeSkipEmpty,
  interposeSkipEmptyBy,
  mintercalateSkipEmpty,
  mintercalateSkipEmptyBy,
  (<+?>),
  munwordsSkipEmpty,
) where

import Data.Foldable
import Data.Monoid.Intercalate
import Data.String

-- | @interposeSkipEmpty s a b@ is like 'interpose', but it puts the
-- separator @s@ only if neither @a@ nor @b@ is 'mempty'. If one of them is
-- 'mempty', the result is the other one.
--
-- >>> interposeSkipEmpty ", " "left" "right"
-- "left, right"
-- >>> interposeSkipEmpty ", " "left" ""
-- "left"
interposeSkipEmpty :: (Eq s, Monoid s) => s -> s -> s -> s
interposeSkipEmpty = interposeSkipEmptyBy (== mempty)

-- | @interposeSkipEmptyBy skip s a b@ is like 'interposeSkipEmpty', but it
-- leaves out each operand for which @skip@ gives 'True'. If it leaves out
-- both, the result is 'mempty'.
--
-- >>> import Data.Char
-- >>> interposeSkipEmptyBy (all isSpace) ", " "left" "  "
-- "left"
-- >>> interposeSkipEmptyBy (all isSpace) ", " " " "  "
-- ""
interposeSkipEmptyBy :: Monoid s => (s -> Bool) -> s -> s -> s -> s
interposeSkipEmptyBy skip s left right
  | skip left = if skip right then mempty else right
  -- The result starts with left before skip looks at right. So on a lazy
  -- type, e.g. String, a right fold over an infinite list streams.
  | otherwise = left <> if skip right then mempty else s <> right

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
mintercalateSkipEmpty = mintercalateSkipEmptyBy (== mempty)

-- | @mintercalateSkipEmptyBy skip s xs@ is like 'mintercalateSkipEmpty',
-- but it leaves out the elements for which @skip@ gives 'True'.
--
-- >>> import Data.Char
-- >>> mintercalateSkipEmptyBy (all isSpace) ", " ["a", " ", "b"]
-- "a, b"
mintercalateSkipEmptyBy :: (Foldable f, Monoid a) => (a -> Bool) -> a -> f a -> a
mintercalateSkipEmptyBy skip s = mintercalate s . filter (not . skip) . toList

-- | @a \<+?> b@ is like @a \<+> b@, but it puts the space only if neither
-- @a@ nor @b@ is 'mempty'. If one of them is 'mempty', the result is the
-- other one.
--
-- >>> "foo" <+?> "bar"
-- "foo bar"
-- >>> "" <+?> "bar"
-- "bar"
--
-- Every grouping of @a \<+?> b \<+?> c@ gives the same result on a type
-- where a join of two non-empty values is never 'mempty', e.g. 'String',
-- @Text@ or @ByteString@.
--
-- The operator has the same fixity as '<>' and '<+>', i.e. @infixr 6@.
(<+?>) :: (Eq s, Monoid s, IsString s) => s -> s -> s
(<+?>) = interposeSkipEmpty space

infixr 6 <+?>

-- | @munwordsSkipEmpty xs@ is like 'munwords', but it leaves out the
-- elements of @xs@ that are 'mempty'. It is 'Data.List.unwords' after the
-- empty strings are removed.
--
-- >>> munwordsSkipEmpty ["foo", "", "bar"]
-- "foo bar"
munwordsSkipEmpty :: (Foldable f, Eq a, Monoid a, IsString a) => f a -> a
munwordsSkipEmpty = mintercalateSkipEmpty space
