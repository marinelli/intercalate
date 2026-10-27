module Main (main) where

import Control.DeepSeq
import Data.ByteString.Char8 qualified as B
import Data.List
import Data.Monoid.Intercalate
import Data.Text qualified as T
import Test.Tasty.Bench

main :: IO ()
main =
  defaultMain
    [ versus "String" "Data.List.intercalate" intercalate id
    , versus "Text" "T.intercalate" T.intercalate T.pack
    , versus "ByteString" "B.intercalate" B.intercalate B.pack
    ]
  where
    versus :: (NFData a, Monoid a) => String -> String -> (a -> [a] -> a) -> (String -> a) -> Benchmark
    versus name baselineName baseline pack =
      env (pure (map pack pieces)) $ \xs ->
        bgroup
          name
          [ bench baselineName (nf (baseline (pack ", ")) xs)
          , -- The pattern looks only at the last name in the path, so each
            -- baseline name must be unique.
            bcompare ("$NF == \"" ++ baselineName ++ "\"") (bench "mintercalate" (nf (mintercalate (pack ", ")) xs))
          ]

    pieces :: [String]
    pieces = map show [1 .. count]

    -- At this count a quadratic join stands out. On strict Text,
    -- foldr1 (interpose s) took 2.4 s and mintercalate 10 ms, in one run
    -- on GHC 9.12.4.
    count :: Int
    count = 100000
