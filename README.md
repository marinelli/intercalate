# intercalate

intercalate joins `Monoid` values and puts a separator between them. The values can be in any `Foldable`, e.g. a list or a `NonEmpty`. They can have any `Monoid` type, e.g. `String`, `Text`, a ByteString `Builder` or `Sum Int`. The package depends only on `base`.

The module `Data.Monoid.Intercalate` has `mintercalate`. It is `Data.List.intercalate` for any `Foldable` of `Monoid` values. The module also has `munwords` and the space operator `(<+>)`.

The module `Data.Monoid.Intercalate.SkipEmpty` has variants that leave out empty values, and the space operator `(<+?>)`. The names of the functions are the same as in `Data.Monoid.Intercalate`, so import the module qualified.

```haskell
>>> import Data.Monoid.Intercalate
>>> import Data.Monoid.Intercalate.SkipEmpty qualified as SkipEmpty
>>> mintercalate ", " ["a", "b", "c"]
"a, b, c"
>>> "foo" <+> "" SkipEmpty.<+?> "bar"
"foo bar"
>>> SkipEmpty.munwords ["foo", "", "bar"]
"foo bar"
```

The package uses the BSD-3-Clause license. See [LICENSE](LICENSE).
