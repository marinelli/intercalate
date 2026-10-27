-- | Variants of the functions in "Data.Monoid.Intercalate" that leave out
-- empty values. They never put a separator next to an empty value. The
-- names are the same, so import this module qualified:
--
-- > import Data.Monoid.Intercalate
-- > import Data.Monoid.Intercalate.SkipEmpty qualified as SkipEmpty
-- >
-- > SkipEmpty.mintercalate ", " ["a", "", "b"]
-- > "SELECT" <+> columns SkipEmpty.<+?> conditions
--
-- The plain functions find the empty values with 'Eq'. The functions that
-- end in @By@ take a predicate instead, for a type without an 'Eq'
-- instance.
module Data.Monoid.Intercalate.SkipEmpty (
  interpose,
  interposeBy,
  mintercalate,
  mintercalateBy,
  (<+?>),
  munwords,
) where

import Data.Foldable
import Data.Monoid.Intercalate qualified as Core
import Data.String

-- | @interpose s a b@ puts the separator @s@ between @a@ and @b@ only if
-- neither of them is 'mempty'. If one of them is 'mempty', the result is
-- the other one.
--
-- >>> interpose ", " "left" "right"
-- "left, right"
-- >>> interpose ", " "left" ""
-- "left"
interpose :: (Eq s, Monoid s) => s -> s -> s -> s
interpose = interposeBy (== mempty)

-- | @interposeBy skip s a b@ is like 'interpose', but it leaves out each
-- operand for which @skip@ gives 'True'. If it leaves out both, the result
-- is 'mempty'.
--
-- >>> import Data.Char
-- >>> interposeBy (all isSpace) ", " "left" "  "
-- "left"
-- >>> interposeBy (all isSpace) ", " " " "  "
-- ""
interposeBy :: Monoid s => (s -> Bool) -> s -> s -> s -> s
interposeBy skip s left right
  | skip left = if skip right then mempty else right
  -- The result starts with left before skip looks at right. So on a lazy
  -- type, e.g. String, a right fold over an infinite list streams.
  | otherwise = left <> if skip right then mempty else s <> right

-- | @mintercalate s xs@ puts the separator @s@ between each pair of
-- neighboring elements of @xs@ that are not 'mempty', and concatenates
-- the result. So the result never has two separators side by side, or a
-- separator at either end.
--
-- >>> mintercalate ", " ["a", "", "b", ""]
-- "a, b"
--
-- If a join of two non-empty values is never 'mempty', e.g. on 'String',
-- @Text@ or @ByteString@, it is a right fold of 'interpose'. On an
-- infinite list whose elements are all 'mempty', it does not terminate.
mintercalate :: (Foldable f, Eq a, Monoid a) => a -> f a -> a
mintercalate = mintercalateBy (== mempty)

-- | @mintercalateBy skip s xs@ is like 'mintercalate', but it leaves out
-- the elements for which @skip@ gives 'True'.
--
-- >>> import Data.Char
-- >>> mintercalateBy (all isSpace) ", " ["a", " ", "b"]
-- "a, b"
mintercalateBy :: (Foldable f, Monoid a) => (a -> Bool) -> a -> f a -> a
mintercalateBy skip s = Core.mintercalate s . filter (not . skip) . toList

-- | @a \<+?> b@ is like @a \<+> b@ from "Data.Monoid.Intercalate", but it
-- puts the space only if neither @a@ nor @b@ is 'mempty'. If one of them
-- is 'mempty', the result is the other one.
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
-- The operator has the same fixity as '<>' and @\<+>@, i.e. @infixr 6@.
(<+?>) :: (Eq s, Monoid s, IsString s) => s -> s -> s
(<+?>) = interpose Core.space

infixr 6 <+?>

-- | @munwords xs@ joins the elements of @xs@ that are not 'mempty' with a
-- single space between each pair of them. It is 'Data.List.unwords' after
-- the empty strings are removed.
--
-- >>> munwords ["foo", "", "bar"]
-- "foo bar"
munwords :: (Foldable f, Eq a, Monoid a, IsString a) => f a -> a
munwords = mintercalate Core.space
