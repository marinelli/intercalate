module Main (main) where

import Control.Monad (unless)
import Data.String.Intercalate
import System.Exit (exitFailure)

-- | The examples from the Haddock documentation, plus the edge cases.
tests :: [(String, String, String)]
tests =
  [ ("interpose", interpose ", " "left" "right", "left, right")
  , ("mintercalate", mintercalate ", " ["a", "b", "c"], "a, b, c")
  , ("mintercalate empty", mintercalate ", " [], "")
  , ("mintercalate singleton", mintercalate ", " ["a"], "a")
  , ("<+>", "foo" <+> "bar", "foo bar")
  , ("smintercalate", smintercalate "," ["a", "b", "c"], "a , b , c")
  , ("smconcat", smconcat ["foo", "bar", "baz"], "foo bar baz")
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
