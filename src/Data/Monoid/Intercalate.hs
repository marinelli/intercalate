-- | Functions that join 'Monoid' values with a separator.
-- "Data.Monoid.Intercalate.SkipEmpty" has the same functions, but they
-- leave out empty values.
module Data.Monoid.Intercalate (
  interpose,
  mintercalate,
  mintercalateMap,
  space,
  (<+>),
  munwords,
) where

import Data.Foldable
import Data.List
import Data.String

-- | @interpose s a b@ puts the separator @s@ between @a@ and @b@.
--
-- >>> interpose ", " "left" "right"
-- "left, right"
interpose :: Semigroup s => s -> s -> s -> s
interpose s left right = left <> s <> right

-- | @mintercalate s xs@ puts @s@ between the elements of @xs@ and
-- concatenates them. It is 'Data.List.intercalate' for any 'Foldable' of
-- 'Monoid' values.
--
-- >>> mintercalate ", " ["a", "b", "c"]
-- "a, b, c"
--
-- For speed on a strict @ByteString@ or a @ShortByteString@, use the
-- @intercalate@ of its own module, which copies the pieces into one buffer.
-- On 100,000 short pieces, it was about 5 times faster on a @ByteString@
-- and 6 times on a @ShortByteString@, with 12 times less memory.
-- On the other types of @text@ and @bytestring@, the speed was the same.
-- The benchmark of this package gave these numbers, with each
-- group run alone on GHC 9.12.4 and bytestring-0.12.2.0.
mintercalate :: (Foldable f, Monoid a) => a -> f a -> a
mintercalate s = mconcat . intersperse s . toList

-- | @mintercalateMap s f xs@ applies @f@ to each element of @xs@ and joins
-- the results with 'mintercalate'.
--
-- >>> mintercalateMap ", " show [1, 2, 3 :: Int]
-- "1, 2, 3"
mintercalateMap :: (Foldable f, Monoid b) => b -> (a -> b) -> f a -> b
mintercalateMap s f = mintercalate s . map f . toList

-- | A single space, as a value of any 'IsString' type.
space :: IsString s => s
space = fromString [' ']

-- | @a \<+> b@ joins @a@ and @b@ with a space between them.
--
-- >>> "foo" <+> "bar"
-- "foo bar"
--
-- Every grouping of @a \<+> b \<+> c@ gives the same result. The chain
-- parses as @a \<+> (b \<+> c)@, which takes linear time on a 'String'.
--
-- The fixity is @infixr 6@, as for '<>', so you can mix the two operators
-- without parentheses.
--
-- "Control.Arrow" also exports a @(\<+>)@. If you import both, hide one.
(<+>) :: (Semigroup s, IsString s) => s -> s -> s
(<+>) = interpose space

infixr 6 <+>

-- | @munwords xs@ joins the elements of @xs@ with a space between them. It
-- is 'Data.List.unwords' for any 'Foldable' of 'Monoid' values.
--
-- >>> munwords ["foo", "bar", "baz"]
-- "foo bar baz"
munwords :: (Foldable f, Monoid a, IsString a) => f a -> a
munwords = mintercalate space
