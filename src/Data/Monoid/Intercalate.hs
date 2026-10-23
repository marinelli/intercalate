-- | Combinators for joining 'Semigroup'\/'Monoid' values together with a
-- separator.
module Data.Monoid.Intercalate (
  interpose,
  mintercalate,
) where

-- | Join two values by placing a separator between them.
--
-- >>> interpose ", " "left" "right"
-- "left, right"
interpose :: Semigroup s => s -> s -> s -> s
interpose s left right = left <> s <> right

-- | Like 'foldr1', but total: folds a 'Foldable' of 'Monoid' values from
-- the right with the given combining function, returning 'mempty' for
-- an empty structure instead of throwing an error.
foldr1Monoid :: (Foldable f, Monoid a) => (a -> a -> a) -> f a -> a
foldr1Monoid f xs = if null xs then mempty else foldr1 f xs

-- | 'Data.List.intercalate' generalised to any 'Foldable' of 'Monoid'
-- values: joins the elements together, inserting the given separator
-- between each adjacent pair. Returns 'mempty' for an empty structure.
--
-- >>> mintercalate ", " ["a", "b", "c"]
-- "a, b, c"
mintercalate :: (Foldable f, Monoid a) => a -> f a -> a
mintercalate = foldr1Monoid . interpose
