-- | Combinators for joining 'Semigroup'\/'Monoid' values together with a
-- separator.
module Data.Monoid.Intercalate (
  interpose,
  mintercalate,
) where

import Data.Foldable (toList)
import Data.List (intersperse)

-- | Join two values by placing a separator between them.
--
-- >>> interpose ", " "left" "right"
-- "left, right"
interpose :: Semigroup s => s -> s -> s -> s
interpose s left right = left <> s <> right

-- | 'Data.List.intercalate' generalised to any 'Foldable' of 'Monoid'
-- values: joins the elements together, inserting the given separator
-- between each adjacent pair. Returns 'mempty' for an empty structure.
--
-- >>> mintercalate ", " ["a", "b", "c"]
-- "a, b, c"
mintercalate :: (Foldable f, Monoid a) => a -> f a -> a
mintercalate s = mconcat . intersperse s . toList
