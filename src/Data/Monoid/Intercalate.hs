-- | Functions that join 'Semigroup' and 'Monoid' values with a separator
-- between them. They work for any 'Monoid'. The functions that separate
-- string-like values with spaces need an 'IsString' type.
--
-- "Data.Monoid.Intercalate.SkipEmpty" has variants that leave out empty
-- values, under the same names.
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

-- | @mintercalate s xs@ puts the separator @s@ between each pair of
-- neighboring elements of @xs@, and concatenates the result. It is
-- 'Data.List.intercalate' for any 'Foldable' of 'Monoid' values. If @xs@
-- is empty, the result is 'mempty'.
--
-- >>> mintercalate ", " ["a", "b", "c"]
-- "a, b, c"
--
-- On a strict @ByteString@ or a @ShortByteString@, use the @intercalate@
-- of its module if speed matters. It copies the pieces straight into one
-- buffer, but this function first builds a list of the pieces and the
-- separators. With 100,000 short pieces, it was about 5 times faster on a
-- strict @ByteString@ and about 6 times faster on a @ShortByteString@. It
-- allocated about 12 times less memory on both. These numbers come from
-- the benchmark of this package, with each group run on its own with
-- GHC 9.12.4 and bytestring-0.12.2.0. The speed ratio depends on the state
-- of the heap: after the other groups, it was about 8 on a strict
-- @ByteString@. On the other types of @text@ and @bytestring@, the speed
-- was the same.
mintercalate :: (Foldable f, Monoid a) => a -> f a -> a
mintercalate s = mconcat . intersperse s . toList

-- | @mintercalateMap s f xs@ applies @f@ to each element of @xs@, and joins
-- the results with 'mintercalate'. It relates to 'mintercalate' as
-- 'foldMap' relates to 'fold'.
--
-- >>> mintercalateMap ", " show [1, 2, 3 :: Int]
-- "1, 2, 3"
mintercalateMap :: (Foldable f, Monoid b) => b -> (a -> b) -> f a -> b
mintercalateMap s f = mintercalate s . map f . toList

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

-- | @munwords xs@ joins the elements of @xs@ with a single space between
-- each pair of neighboring elements. It is 'Data.List.unwords' for any
-- 'Foldable' of values whose type is both a 'Monoid' and an 'IsString'.
--
-- >>> munwords ["foo", "bar", "baz"]
-- "foo bar baz"
munwords :: (Foldable f, Monoid a, IsString a) => f a -> a
munwords = mintercalate space
