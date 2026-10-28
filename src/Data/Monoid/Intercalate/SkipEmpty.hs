-- | The functions of "Data.Monoid.Intercalate", but they leave out empty
-- values, so no separator is next to an empty value. The names are the
-- same, so import this module qualified:
--
-- > import Data.Monoid.Intercalate
-- > import Data.Monoid.Intercalate.SkipEmpty qualified as SkipEmpty
-- >
-- > SkipEmpty.mintercalate ", " ["a", "", "b"]
-- > "SELECT" <+> columns SkipEmpty.<+?> conditions
--
-- The plain functions find empty values with 'Eq'. The @By@ functions take
-- a predicate instead, e.g. for a type without 'Eq'.
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

-- | @interpose s a b@ puts @s@ between @a@ and @b@ only if neither is
-- 'mempty'. If one is 'mempty', the result is the other one.
--
-- >>> interpose ", " "left" "right"
-- "left, right"
-- >>> interpose ", " "left" ""
-- "left"
interpose :: (Eq s, Monoid s) => s -> s -> s -> s
interpose = interposeBy (== mempty)

-- | @interposeBy skip s a b@ is like 'interpose', but it leaves out each
-- operand for which @skip@ is 'True'. If it leaves out both, the result is
-- 'mempty'.
--
-- >>> import Data.Char
-- >>> interposeBy (all isSpace) ", " "left" "  "
-- "left"
-- >>> interposeBy (all isSpace) ", " " " "  "
-- ""
interposeBy :: Monoid s => (s -> Bool) -> s -> s -> s -> s
interposeBy skip s left right
  | skip left = if skip right then mempty else right
  -- This emits left before it tests right. So a right fold over an
  -- infinite list of strings still produces output.
  | otherwise = left <> if skip right then mempty else s <> right

-- | @mintercalate s xs@ puts @s@ between the elements of @xs@ that are not
-- 'mempty', and concatenates them.
--
-- >>> mintercalate ", " ["a", "", "b", ""]
-- "a, b"
--
-- If a join of two non-empty values is never 'mempty', e.g. on 'String',
-- it is a right fold of 'interpose'. On an infinite list of 'mempty'
-- values, it does not terminate.
mintercalate :: (Foldable f, Eq a, Monoid a) => a -> f a -> a
mintercalate = mintercalateBy (== mempty)

-- | @mintercalateBy skip s xs@ is like 'mintercalate', but it leaves out
-- the elements for which @skip@ is 'True'.
--
-- >>> import Data.Char
-- >>> mintercalateBy (all isSpace) ", " ["a", " ", "b"]
-- "a, b"
mintercalateBy :: (Foldable f, Monoid a) => (a -> Bool) -> a -> f a -> a
mintercalateBy skip s = Core.mintercalate s . filter (not . skip) . toList

-- | @a \<+?> b@ is like @a \<+> b@ in "Data.Monoid.Intercalate", but it
-- puts the space only if neither operand is 'mempty'.
--
-- >>> "foo" <+?> "bar"
-- "foo bar"
-- >>> "" <+?> "bar"
-- "bar"
--
-- Every grouping of @a \<+?> b \<+?> c@ gives the same result, if a join
-- of two non-empty values is never 'mempty', e.g. on 'String'.
--
-- The fixity is @infixr 6@, as for '<>' and @\<+>@. You can mix the
-- operators, but @\<+>@ always puts its space. So use @\<+>@ only next to an
-- operand that is never 'mempty':
--
-- >>> import Data.Monoid.Intercalate
-- >>> "foo" <+?> "" <+> "bar"
-- "foo  bar"
(<+?>) :: (Eq s, Monoid s, IsString s) => s -> s -> s
(<+?>) = interpose Core.space

infixr 6 <+?>

-- | @munwords xs@ joins the elements of @xs@ that are not 'mempty' with a
-- space between them.
--
-- >>> munwords ["foo", "", "bar"]
-- "foo bar"
munwords :: (Foldable f, Eq a, Monoid a, IsString a) => f a -> a
munwords = mintercalate Core.space
