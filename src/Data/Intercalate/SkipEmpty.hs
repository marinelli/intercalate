{-# LANGUAGE DefaultSignatures #-}

-- | The functions of "Data.Monoid.Intercalate.SkipEmpty", with the same
-- names, for the types of the 'IsEmpty' and 'Intercalate' classes. They
-- find the empty values with 'isEmpty', and they join with the fastest
-- join of each type.
module Data.Intercalate.SkipEmpty (
  IsEmpty (..),
  interposeSkipEmpty,
  Core.interposeSkipEmptyBy,
  mintercalateSkipEmpty,
  mintercalateSkipEmptyBy,
  (<+?>),
  munwordsSkipEmpty,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as SBS
import Data.Foldable
import Data.Intercalate
import Data.Monoid.Intercalate.SkipEmpty qualified as Core
import Data.String
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL

-- | A 'Monoid' with a test for empty values.
--
-- An instance must satisfy these laws:
--
-- * @'isEmpty' 'mempty' == True@
--
-- * @'isEmpty' (x '<>' y) == ('isEmpty' x && 'isEmpty' y)@
--
-- * @'isEmpty' x == (x == 'mempty')@, if the type has an 'Eq' instance.
--
-- If the type has an 'Eq' instance, an empty instance declaration is
-- enough:
--
-- > instance IsEmpty MyType
--
-- Write 'isEmpty' for a type whose empty value has more than one form,
-- e.g. a query that is a sequence of text chunks.
class Monoid a => IsEmpty a where
  -- | @isEmpty x@ is 'True' if @x@ is empty.
  isEmpty :: a -> Bool
  default isEmpty :: Eq a => a -> Bool
  isEmpty = (== mempty)

-- The test needs no Eq instance for the elements.
instance IsEmpty [c] where
  isEmpty = null

instance IsEmpty T.Text

instance IsEmpty TL.Text

instance IsEmpty BS.ByteString

instance IsEmpty BL.ByteString

instance IsEmpty SBS.ShortByteString

-- | @interposeSkipEmpty s a b@ is like 'interpose', but it puts the
-- separator @s@ only if neither @a@ nor @b@ is empty. If one of them is
-- empty, the result is the other one.
--
-- >>> interposeSkipEmpty ", " "left" ""
-- "left"
interposeSkipEmpty :: IsEmpty s => s -> s -> s -> s
interposeSkipEmpty = Core.interposeSkipEmptyBy isEmpty

-- | @mintercalateSkipEmpty s xs@ is like 'mintercalate', but it leaves out
-- the elements of @xs@ that are empty.
--
-- >>> mintercalateSkipEmpty ", " ["a", "", "b", ""]
-- "a, b"
mintercalateSkipEmpty :: (Foldable f, IsEmpty a, Intercalate a) => a -> f a -> a
mintercalateSkipEmpty = mintercalateSkipEmptyBy isEmpty

-- | @mintercalateSkipEmptyBy skip s xs@ is like 'mintercalateSkipEmpty',
-- but it leaves out the elements for which @skip@ gives 'True'.
--
-- >>> import Data.Char
-- >>> mintercalateSkipEmptyBy (all isSpace) ", " ["a", " ", "b"]
-- "a, b"
mintercalateSkipEmptyBy :: (Foldable f, Intercalate a) => (a -> Bool) -> a -> f a -> a
mintercalateSkipEmptyBy skip s = mintercalate s . filter (not . skip) . toList

-- | @a \<+?> b@ is like @a \<+> b@, but it puts the space only if neither
-- @a@ nor @b@ is empty. If one of them is empty, the result is the other
-- one.
--
-- >>> "" <+?> "bar"
-- "bar"
--
-- The operator has the same fixity as '<>' and '<+>', i.e. @infixr 6@.
(<+?>) :: (IsEmpty s, IsString s) => s -> s -> s
(<+?>) = interposeSkipEmpty space

infixr 6 <+?>

-- | @munwordsSkipEmpty xs@ is like 'munwords', but it leaves out the
-- elements of @xs@ that are empty.
--
-- >>> munwordsSkipEmpty ["foo", "", "bar"]
-- "foo bar"
munwordsSkipEmpty :: (Foldable f, IsEmpty a, Intercalate a, IsString a) => f a -> a
munwordsSkipEmpty = mintercalateSkipEmpty space
