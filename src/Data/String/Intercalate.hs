-- | Variants of the functions in "Data.Monoid.Intercalate" that separate
-- string-like values with spaces. A string-like value has an 'IsString'
-- type. You can use these functions to build text output piece by piece,
-- e.g. an expression or a query.
module Data.String.Intercalate (
  module Data.Monoid.Intercalate,
  space,
  (<+>),
  (<+?>),
  smintercalate,
  smconcat,
  smconcatSkipEmpty,
) where

import Data.Monoid.Intercalate
import Data.String

-- | A single space character, as a value of any 'IsString' type.
space :: IsString s => s
space = fromString [' ']

-- | @a \<+> b@ joins @a@ and @b@ with a single space between them.
--
-- >>> "foo" <+> "bar"
-- "foo bar"
--
-- Every grouping of @a \<+> b \<+> c@ gives the same result. Without
-- parentheses, the chain parses as @a \<+> (b \<+> c)@. This order takes
-- linear time on lists such as 'String'.
--
-- The operator has the same fixity as '<>', i.e. @infixr 6@. So you can use
-- the two operators in one expression without parentheses.
--
-- "Control.Arrow" also exports a @(\<+>)@. If you import it together with
-- this module, hide one of the two operators.
(<+>) :: (Semigroup s, IsString s) => s -> s -> s
(<+>) = interpose space

infixr 6 <+>

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

-- | @smintercalate s xs@ is like 'mintercalate', but it puts a space before
-- and after the separator @s@. So no element of @xs@ touches the separator.
--
-- >>> smintercalate "," ["a", "b", "c"]
-- "a , b , c"
smintercalate :: (Foldable f, Monoid a, IsString a) => a -> f a -> a
smintercalate s = mintercalate (interpose s space space)

-- | @smconcat xs@ joins the elements of @xs@ with a single space between
-- each pair of neighboring elements. It is 'Data.List.unwords' for any
-- 'Foldable' of values whose type is both a 'Monoid' and an 'IsString'.
--
-- >>> smconcat ["foo", "bar", "baz"]
-- "foo bar baz"
smconcat :: (Foldable f, Monoid a, IsString a) => f a -> a
smconcat = mintercalate space

-- | @smconcatSkipEmpty xs@ is like 'smconcat', but it leaves out the
-- elements of @xs@ that are 'mempty'. It is 'Data.List.unwords' after the
-- empty strings are removed.
--
-- >>> smconcatSkipEmpty ["foo", "", "bar"]
-- "foo bar"
smconcatSkipEmpty :: (Foldable f, Eq a, Monoid a, IsString a) => f a -> a
smconcatSkipEmpty = mintercalateSkipEmpty space
