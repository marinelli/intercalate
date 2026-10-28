module Main (main) where

import Control.DeepSeq
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.ByteString.Short qualified as SBS
import Data.List
import Data.Monoid.Intercalate
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Test.Tasty.Bench

main :: IO ()
main =
  -- Earlier groups slow down later ones through the heap. On GHC 9.12.4,
  -- mintercalate on ByteString took 6.6 ms alone and 10.9 ms after the
  -- others. The Haddock of mintercalate quotes runs of one group. Run one
  -- group with $(cabal list-bin intercalate-bench) -p '$2 == "ByteString"'.
  -- The --benchmark-options flag of cabal bench splits this pattern at its
  -- spaces.
  defaultMain
    [ versus "String" "Data.List.intercalate" intercalate id
    , versus "Text" "T.intercalate" T.intercalate T.pack
    , versus "LazyText" "TL.intercalate" TL.intercalate TL.pack
    , versus "ByteString" "B.intercalate" B.intercalate B.pack
    , versus "LazyByteString" "BL.intercalate" BL.intercalate BL.pack
    , versus "ShortByteString" "SBS.intercalate" SBS.intercalate (SBS.toShort . B.pack)
    ]
  where
    versus
      :: (NFData a, Monoid a) => String -> String -> (a -> [a] -> a) -> (String -> a) -> Benchmark
    versus name baselineName baseline pack =
      env (pure (map pack pieces)) $ \xs ->
        bgroup
          name
          [ bench baselineName (nf (baseline (pack ", ")) xs)
          , -- The pattern matches only the last name in the path, so the
            -- baseline names must be unique.
            bcompare
              ("$NF == \"" ++ baselineName ++ "\"")
              (bench "mintercalate" (nf (mintercalate (pack ", ")) xs))
          ]

    pieces :: [String]
    pieces = map show [1 .. count]

    -- This count makes a quadratic join stand out. On strict Text,
    -- foldr1 (interpose s) took 24 s and mintercalate 7.4 ms, with
    -- GHC 9.12.4 and text-2.1.4 on an Intel i7-7700.
    count :: Int
    count = 100000
