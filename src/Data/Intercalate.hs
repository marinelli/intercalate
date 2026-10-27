-- | The functions of "Data.Monoid.Intercalate", with the same names, for
-- the types of the 'Intercalate' class. The list functions use the fastest
-- join of each type. To switch between the two modules, change the import.
--
-- "Data.Intercalate.SkipEmpty" has variants that leave out empty values.
module Data.Intercalate (
  Intercalate (..),
  interpose,
  mintercalate,
  mintercalateMap,
  space,
  (<+>),
  munwords,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as SBS
import Data.Foldable
import Data.Monoid.Intercalate hiding (mintercalate, mintercalateMap, munwords)
import Data.Monoid.Intercalate qualified as Core
import Data.String
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL

-- | A 'Monoid' with a join for a list of values.
--
-- An instance must satisfy this law: @'intercalateList' s xs@ equals
-- @mintercalate s xs@ from "Data.Monoid.Intercalate". The default
-- 'intercalateList' is that function. If your type has a faster
-- join, override 'intercalateList' with it. Otherwise an empty instance
-- declaration is enough:
--
-- > instance Intercalate MyType
class Monoid a => Intercalate a where
  -- | @intercalateList s xs@ puts the separator @s@ between each pair of
  -- neighboring elements of @xs@, and concatenates the result.
  intercalateList :: a -> [a] -> a
  intercalateList = Core.mintercalate

instance Intercalate [c]

instance Intercalate T.Text

instance Intercalate TL.Text

instance Intercalate BL.ByteString

instance Intercalate BB.Builder

-- Only these two instances override the default. For the other types, the
-- benchmark of this package measured the same speed with the default.
instance Intercalate BS.ByteString where
  intercalateList = BS.intercalate

instance Intercalate SBS.ShortByteString where
  intercalateList = SBS.intercalate

-- | @mintercalate s xs@ puts the separator @s@ between each pair of
-- neighboring elements of @xs@, and concatenates the result. It is
-- 'Data.List.intercalate' for any 'Foldable' of 'Intercalate' values.
--
-- >>> import Data.ByteString.Char8 qualified as B
-- >>> mintercalate (B.pack ", ") (map B.pack ["a", "b", "c"])
-- "a, b, c"
mintercalate :: (Foldable f, Intercalate a) => a -> f a -> a
mintercalate s = intercalateList s . toList

-- | @mintercalateMap s f xs@ applies @f@ to each element of @xs@, and joins
-- the results with 'mintercalate'.
--
-- >>> mintercalateMap ", " show [1, 2, 3 :: Int]
-- "1, 2, 3"
mintercalateMap :: (Foldable f, Intercalate b) => b -> (a -> b) -> f a -> b
mintercalateMap s f = intercalateList s . map f . toList

-- | @munwords xs@ joins the elements of @xs@ with a single space between
-- each pair of neighboring elements.
--
-- >>> munwords ["foo", "bar", "baz"]
-- "foo bar baz"
munwords :: (Foldable f, Intercalate a, IsString a) => f a -> a
munwords = mintercalate space
