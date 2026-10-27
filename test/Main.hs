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
        , example "interposeSkipEmpty" "left, right" (interposeSkipEmpty ", " "left" "right")
        , example "interposeSkipEmpty empty left" "right" (interposeSkipEmpty ", " "" "right")
        , example "interposeSkipEmpty empty right" "left" (interposeSkipEmpty ", " "left" "")
        , example "mintercalate" "a, b, c" (mintercalate ", " ["a", "b", "c"])
        , example "mintercalate empty" "" (mintercalate ", " [])
        , example "mintercalate singleton" "a" (mintercalate ", " ["a"])
        , example "mintercalate Maybe" "a" (mintercalate ", " (Just "a"))
        , example "mintercalate NonEmpty" "a, b, c" (mintercalate ", " ("a" NE.:| ["b", "c"]))
        , example "mintercalate Text" "a, b, c" (T.unpack (mintercalate (T.pack ", ") (map T.pack ["a", "b", "c"])))
        , example "<+>" "foo bar" ("foo" <+> "bar")
        , example "<+> has the fixity of <>" $(reifyFixity '(<>) >>= stringE . show) $(reifyFixity '(<+>) >>= stringE . show)
        , example "<+?>" "foo bar" ("foo" <+?> "bar")
        , example "<+?> empty left" "bar" ("" <+?> "bar")
        , example "<+?> empty right" "foo" ("foo" <+?> "")
        , example "<+?> Text" "bar" (T.unpack (T.empty <+?> T.pack "bar"))
        , example "<+?> is lazy" "x y" (take 3 (foldr (<+?>) "" ("x" : "" : "y" : undefined)))
        , example "<+?> has the fixity of <>" $(reifyFixity '(<>) >>= stringE . show) $(reifyFixity '(<+?>) >>= stringE . show)
        , example "smintercalate" "a , b , c" (smintercalate "," ["a", "b", "c"])
        , example "smconcat" "foo bar baz" (smconcat ["foo", "bar", "baz"])
        , example "smconcat Text" "foo bar baz" (T.unpack (smconcat (map T.pack ["foo", "bar", "baz"])))
        , -- With repeat "x", a spine-strict version would loop without
          -- allocating, and tasty's timeout could not stop it. The list ends right
          -- after "y", so a version that looks one element ahead also fails.
          example "smconcat is lazy" "x y" (take 3 (smconcat ("x" : "y" : undefined)))
        , example "mintercalateSkipEmpty" "a, b" (mintercalateSkipEmpty ", " ["", "a", "", "b", ""])
        , example "mintercalateSkipEmpty only empty" "" (mintercalateSkipEmpty ", " ["", ""])
        , example "mintercalateSkipEmpty Text" "a, b" (T.unpack (mintercalateSkipEmpty (T.pack ", ") (map T.pack ["a", "", "b"])))
        , example "mintercalateSkipEmpty is lazy" "x y" (take 3 (mintercalateSkipEmpty " " ("x" : "" : "y" : undefined)))
        , example "smconcatSkipEmpty" "foo bar" (smconcatSkipEmpty ["foo", "", "bar"])
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
        , testProperty "interposeSkipEmpty is intercalate without empty operands" skipsEmpty
        , testProperty "<+?> is associative" associativeNonEmpty
        , testProperty "mintercalateSkipEmpty is intercalate without empty elements" skipsEmptyElements
        , testProperty "mintercalateSkipEmpty is a right fold of interposeSkipEmpty" foldsInterposeSkipEmpty
        , testProperty "smconcatSkipEmpty is unwords without empty elements" unwordsSkipEmpty
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

        skipsEmpty :: String -> String -> String -> Property
        skipsEmpty s a b = interposeSkipEmpty s a b === intercalate s (filter (not . null) [a, b])

        associativeNonEmpty :: String -> String -> String -> Property
        associativeNonEmpty a b c = (a <+?> b) <+?> c === a <+?> (b <+?> c)

        skipsEmptyElements :: String -> [String] -> Property
        skipsEmptyElements s xs = mintercalateSkipEmpty s xs === intercalate s (filter (not . null) xs)

        foldsInterposeSkipEmpty :: String -> [String] -> Property
        foldsInterposeSkipEmpty s xs = mintercalateSkipEmpty s xs === foldr (interposeSkipEmpty s) "" xs

        unwordsSkipEmpty :: [String] -> Property
        unwordsSkipEmpty xs = smconcatSkipEmpty xs === unwords (filter (not . null) xs)
