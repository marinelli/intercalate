{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.ByteString.Char8 qualified as B
import Data.List
import Data.List.NonEmpty qualified as NE
import Data.String.Intercalate
import Data.Text qualified as T
import Language.Haskell.TH
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain (testGroup "intercalate" [examples, properties])
  where
    examples :: TestTree
    examples =
      testGroup
        "examples"
        [ example "interpose" "left, right" (interpose ", " "left" "right")
        , example "mintercalate" "a, b, c" (mintercalate ", " ["a", "b", "c"])
        , example "mintercalate empty" "" (mintercalate ", " [])
        , example "mintercalate singleton" "a" (mintercalate ", " ["a"])
        , example "mintercalate Maybe" "a" (mintercalate ", " (Just "a"))
        , example "mintercalate NonEmpty" "a, b, c" (mintercalate ", " ("a" NE.:| ["b", "c"]))
        , example "mintercalate Text" "a, b, c" (T.unpack (mintercalate (T.pack ", ") (map T.pack ["a", "b", "c"])))
        , example "<+>" "foo bar" ("foo" <+> "bar")
        , example "<+> has the fixity of <>" $(reifyFixity '(<>) >>= stringE . show) $(reifyFixity '(<+>) >>= stringE . show)
        , example "smintercalate" "a , b , c" (smintercalate "," ["a", "b", "c"])
        , example "smconcat" "foo bar baz" (smconcat ["foo", "bar", "baz"])
        , example "smconcat Text" "foo bar baz" (T.unpack (smconcat (map T.pack ["foo", "bar", "baz"])))
        , example "smconcat infinite" "x x x" (take 5 (smconcat (repeat "x")))
        ]
      where
        example :: String -> String -> String -> TestTree
        example name expected actual = testCase name (assertEqual "" expected actual)

    properties :: TestTree
    properties =
      testGroup
        "properties"
        [ testProperty "mintercalate is Data.List.intercalate" (agreesWith id intercalate)
        , testProperty "mintercalate is T.intercalate" (agreesWith T.pack T.intercalate)
        , testProperty "mintercalate is B.intercalate" (agreesWith B.pack B.intercalate)
        , testProperty "smintercalate pads the separator" paddedSeparator
        , testProperty "smconcat is unwords" unwordsSmconcat
        , testProperty "<+> is associative" associative
        ]
      where
        agreesWith :: (Eq a, Show a, Monoid a) => (String -> a) -> (a -> [a] -> a) -> String -> [String] -> Property
        agreesWith pack baseline s xs = mintercalate (pack s) (map pack xs) === baseline (pack s) (map pack xs)

        paddedSeparator :: String -> [String] -> Property
        paddedSeparator s xs = smintercalate s xs === intercalate (" " ++ s ++ " ") xs

        unwordsSmconcat :: [String] -> Property
        unwordsSmconcat xs = smconcat xs === unwords xs

        associative :: String -> String -> String -> Property
        associative a b c = (a <+> b) <+> c === a <+> (b <+> c)
