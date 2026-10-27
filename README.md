# intercalate

intercalate joins `Monoid` values and puts a separator between them. The values can be in any `Foldable`, e.g. a list or a `NonEmpty`.

The package has four modules:

- `Data.Monoid.Intercalate` works for any `Monoid`, e.g. `String`, `Text`, a ByteString `Builder` or `Sum Int`. Its `mintercalate` is `Data.List.intercalate` for any `Foldable` of `Monoid` values, and its `munwords` is `Data.List.unwords`.
- `Data.Intercalate` has the same functions for the types of the `Intercalate` class. They use the fastest join of each type, e.g. `Data.ByteString.intercalate`.
- `Data.Monoid.Intercalate.SkipEmpty` and `Data.Intercalate.SkipEmpty` add variants that leave out empty values. They never put a separator next to an empty value.

```haskell
>>> import Data.Monoid.Intercalate
>>> import Data.Monoid.Intercalate.SkipEmpty
>>> mintercalate ", " ["a", "b", "c"]
"a, b, c"
>>> "foo" <+> "bar"
"foo bar"
>>> munwordsSkipEmpty ["foo", "", "bar"]
"foo bar"
```

To use the fast joins, import `Data.Intercalate` and `Data.Intercalate.SkipEmpty` instead. The function names stay the same.

The package uses the BSD-3-Clause license. See [LICENSE](LICENSE).
