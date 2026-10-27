# intercalate

intercalate joins `Monoid` values and puts a separator between them. The values can be in any `Foldable`, e.g. a list or a `NonEmpty`. They can have any `Monoid` type, e.g. `String`, `Text`, a ByteString `Builder` or `Sum Int`. The package depends only on `base`.

The module `Data.Monoid.Intercalate` has `mintercalate`. It is `Data.List.intercalate` for any `Foldable` of `Monoid` values. The module also has `munwords`, the `(<+>)` operator, and variants that leave out empty values, e.g. `mintercalateSkipEmpty` and `(<+?>)`.

```haskell
>>> import Data.Monoid.Intercalate
>>> mintercalate ", " ["a", "b", "c"]
"a, b, c"
>>> "foo" <+> "bar"
"foo bar"
>>> munwordsSkipEmpty ["foo", "", "bar"]
"foo bar"
```

The package uses the BSD-3-Clause license. See [LICENSE](LICENSE).
