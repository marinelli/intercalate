module Main (main) where

import Control.Monad
import Data.List.NonEmpty qualified as NE
import Data.String.Intercalate
import Data.Text qualified as T
import System.Exit

-- | Each test holds a name, the actual result and the expected result.
tests :: [(String, String, String)]
tests =
  [ ("interpose", interpose ", " "left" "right", "left, right")
  , ("mintercalate", mintercalate ", " ["a", "b", "c"], "a, b, c")
  , ("mintercalate empty", mintercalate ", " [], "")
  , ("mintercalate singleton", mintercalate ", " ["a"], "a")
  , ("mintercalate Maybe", mintercalate ", " (Just "a"), "a")
  , ("mintercalate NonEmpty", mintercalate ", " ("a" NE.:| ["b", "c"]), "a, b, c")
  , ("mintercalate Text", T.unpack (mintercalate (T.pack ", ") (map T.pack ["a", "b", "c"])), "a, b, c")
  , ("<+>", "foo" <+> "bar", "foo bar")
  , -- If (<+>) were infixl 6, this test would not compile, because (<>) is infixr 6.
    ("<+> mixed with <>", "a" <> "b" <+> "c" <> "d", "ab cd")
  , ("smintercalate", smintercalate "," ["a", "b", "c"], "a , b , c")
  , ("smconcat", smconcat ["foo", "bar", "baz"], "foo bar baz")
  , ("smconcat Text", T.unpack (smconcat (map T.pack ["foo", "bar", "baz"])), "foo bar baz")
  , ("smconcat infinite", take 5 (smconcat (repeat "x")), "x x x")
  ]

main :: IO ()
main = do
  let failures = [t | t@(_, actual, expected) <- tests, actual /= expected]
  mapM_ report failures
  unless (null failures) exitFailure
  where
    report (name, actual, expected) =
      putStrLn $ name ++ ": expected " ++ show expected ++ ", got " ++ show actual
