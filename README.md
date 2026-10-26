# intercalate

intercalate joins `Monoid` values and puts a separator between them. The values can be in any `Foldable`, e.g. a list or a `NonEmpty`. They can have any `Monoid` type, e.g. `String`, `Text` or `ByteString`.

The module `Data.Monoid.Intercalate` has `mintercalate`. It is `Data.List.intercalate` for any `Foldable` of `Monoid` values. The module `Data.String.Intercalate` adds variants that separate the values with spaces.

```haskell
>>> import Data.String.Intercalate
>>> mintercalate ", " ["a", "b", "c"]
"a, b, c"
>>> "foo" <+> "bar"
"foo bar"
>>> smconcat ["foo", "bar", "baz"]
"foo bar baz"
```

The package uses the BSD-3-Clause license. See [LICENSE](LICENSE).
