# Revision history for intercalate

## 1.0.0.0 -- unreleased

* First version.
* `Data.Monoid.Intercalate` provides `interpose`, `mintercalate`, `mintercalateMap`, `space`, `(<+>)` and `munwords` for any `Monoid`.
* `Data.Monoid.Intercalate.SkipEmpty` provides `interposeSkipEmpty`, `interposeSkipEmptyBy`, `mintercalateSkipEmpty`, `mintercalateSkipEmptyBy`, `(<+?>)` and `munwordsSkipEmpty`. They leave out empty values.
* `Data.Intercalate` and `Data.Intercalate.SkipEmpty` provide the same functions with fast joins. They support `String`, strict and lazy `Text`, strict and lazy `ByteString`, `ShortByteString` and ByteString `Builder`. The classes `Intercalate` and `IsEmpty` let you add your own types.
