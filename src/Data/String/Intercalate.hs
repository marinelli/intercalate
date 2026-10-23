-- | Convenience variants of "Data.Monoid.Intercalate" for the common case
-- of separating string-like values with plain spaces. Useful for building
-- up textual output (e.g. rendering an expression or query) fragment by
-- fragment.
module Data.String.Intercalate (
  module Data.Monoid.Intercalate,
  space,
  (<+>),
  smintercalate,
  smconcat,
) where

import Data.Monoid.Intercalate
import Data.String

-- | A single space, as any 'IsString' value.
space :: IsString s => s
space = fromString [' ']

-- | Concatenate two values, separated by a single space.
--
-- >>> "foo" <+> "bar"
-- "foo bar"
--
-- Right-associative, so a chain @a <+> b <+> c@ reads left to right as
-- a single space-separated sequence.
(<+>) :: (Semigroup s, IsString s) => s -> s -> s
(<+>) = interpose space

infixr 6 <+>

-- | Like 'mintercalate', but pads the separator with a space on each
-- side before intercalating, so neighbouring elements never run
-- straight into the separator.
--
-- >>> smintercalate "," ["a", "b", "c"]
-- "a , b , c"
smintercalate :: (Foldable f, Monoid a, IsString a) => a -> f a -> a
smintercalate = mintercalate . (\s -> interpose s space space)

-- | Concatenate a 'Foldable' of values, separated by single spaces.
-- Equivalent to 'Data.List.unwords', generalised to any
-- 'Monoid'\/'IsString' type.
--
-- >>> smconcat ["foo", "bar", "baz"]
-- "foo bar baz"
smconcat :: (Foldable f, Monoid a, IsString a) => f a -> a
smconcat = mintercalate space
