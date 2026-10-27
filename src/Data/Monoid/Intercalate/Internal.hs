-- | Code that "Data.Monoid.Intercalate" and
-- "Data.Monoid.Intercalate.SkipEmpty" share. The second module imports the
-- first, so the shared code cannot live in either of them.
module Data.Monoid.Intercalate.Internal (
  interposeSkipBy,
) where

-- | @interposeSkipBy skip s a b@ puts the separator @s@ between @a@ and @b@,
-- but it leaves out each operand for which @skip@ gives 'True'.
interposeSkipBy :: Monoid s => (s -> Bool) -> s -> s -> s -> s
interposeSkipBy skip s left right
  | skip left = if skip right then mempty else right
  -- The result starts with left before skip looks at right. So on a lazy
  -- type, e.g. String, a right fold over an infinite list streams.
  | otherwise = left <> if skip right then mempty else s <> right
