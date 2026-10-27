{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.ByteString.Char8 qualified as B
import Data.Char
import Data.List
import Data.List.NonEmpty qualified as NE
import Data.Monoid.Intercalate
import Data.Monoid.Intercalate.SkipEmpty qualified as SkipEmpty
import Data.Text qualified as T
import Language.Haskell.TH
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- | A short string that is often empty or blank. Arbitrary strings are
-- rarely empty, so the properties of the skip-empty functions would see
-- few empty values.
newtype Piece = Piece {unPiece :: String}

instance Show Piece where
  show = show . unPiece

instance Arbitrary Piece where
  arbitrary = Piece <$> frequency [(1, pure ""), (3, listOf (elements "ab, "))]
  shrink = map Piece . shrink . unPiece

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
        , example "mintercalate is lazy" "a" (take 1 (mintercalate "," ("a" : undefined)))
        , example "mintercalateMap" "1, 2, 3" (mintercalateMap ", " show [1, 2, 3 :: Int])
        , example "<+>" "foo bar" ("foo" <+> "bar")
        , example "<+> has the fixity of <>" $(reifyFixity '(<>) >>= stringE . show) $(reifyFixity '(<+>) >>= stringE . show)
        , example "munwords" "foo bar baz" (munwords ["foo", "bar", "baz"])
        , example "munwords Text" "foo bar baz" (T.unpack (munwords (map T.pack ["foo", "bar", "baz"])))
        , -- With repeat "x", a spine-strict version would loop without
          -- allocating, and tasty's timeout could not stop it. The list ends right
          -- after "y", so a version that looks one element ahead also fails.
          example "munwords is lazy" "x y" (take 3 (munwords ("x" : "y" : undefined)))
        , example "SkipEmpty.interpose" "left, right" (SkipEmpty.interpose ", " "left" "right")
        , example "SkipEmpty.interpose empty left" "right" (SkipEmpty.interpose ", " "" "right")
        , example "SkipEmpty.interpose empty right" "left" (SkipEmpty.interpose ", " "left" "")
        , example "SkipEmpty.interposeBy blank left" "right" (SkipEmpty.interposeBy (all isSpace) ", " "  " "right")
        , example "SkipEmpty.<+?>" "foo bar" ("foo" SkipEmpty.<+?> "bar")
        , example "SkipEmpty.<+?> empty left" "bar" ("" SkipEmpty.<+?> "bar")
        , example "SkipEmpty.<+?> empty right" "foo" ("foo" SkipEmpty.<+?> "")
        , example "SkipEmpty.<+?> Text" "bar" (T.unpack (T.empty SkipEmpty.<+?> T.pack "bar"))
        , example "SkipEmpty.<+?> mixed with <+>" "SELECT x" ("SELECT" <+> "x" SkipEmpty.<+?> "")
        , example "SkipEmpty.<+?> is lazy" "x y" (take 3 (foldr (SkipEmpty.<+?>) "" ("x" : "" : "y" : undefined)))
        , example "SkipEmpty.<+?> has the fixity of <>" $(reifyFixity '(<>) >>= stringE . show) $(reifyFixity '(SkipEmpty.<+?>) >>= stringE . show)
        , example "SkipEmpty.mintercalate" "a, b" (SkipEmpty.mintercalate ", " ["", "a", "", "b", ""])
        , example "SkipEmpty.mintercalate only empty" "" (SkipEmpty.mintercalate ", " ["", ""])
        , example "SkipEmpty.mintercalate Text" "a, b" (T.unpack (SkipEmpty.mintercalate (T.pack ", ") (map T.pack ["a", "", "b"])))
        , example "SkipEmpty.mintercalate is lazy" "x y" (take 3 (SkipEmpty.mintercalate " " ("x" : "" : "y" : undefined)))
        , example "SkipEmpty.mintercalateBy" "a, b" (SkipEmpty.mintercalateBy (all isSpace) ", " ["a", " ", "", "b"])
        , example "SkipEmpty.munwords" "foo bar" (SkipEmpty.munwords ["foo", "", "bar"])
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
    , testProperty "mintercalateMap maps, then joins" mapsThenJoins
    , testProperty "munwords is unwords" unwordsMunwords
    , testProperty "<+> is associative" associative
    , testProperty "SkipEmpty.interpose is intercalate without empty operands" skipsEmpty
    , testProperty "SkipEmpty.interposeBy is intercalate without skipped operands" skipsBlank
    , testProperty "SkipEmpty.<+?> is associative" associativeSkipEmpty
    , testProperty "SkipEmpty.mintercalate is intercalate without empty elements" skipsEmptyElements
    , testProperty "SkipEmpty.mintercalateBy is intercalate without skipped elements" skipsBlankElements
    , testProperty "SkipEmpty.mintercalate is a right fold of SkipEmpty.interpose" foldsInterposeSkipEmpty
    , testProperty "SkipEmpty.mintercalateBy is a right fold of SkipEmpty.interposeBy" foldsInterposeSkipEmptyBy
    , testProperty "SkipEmpty.munwords is unwords without empty elements" unwordsSkipEmpty
    ]
  where
    agreesWith :: (Eq a, Show a, Monoid a) => (String -> a) -> (a -> [a] -> a) -> Piece -> [Piece] -> Property
    agreesWith pack baseline (Piece s) ps = mintercalate (pack s) xs === baseline (pack s) xs
      where
        xs = map (pack . unPiece) ps

    mapsThenJoins :: Piece -> [Piece] -> Property
    mapsThenJoins (Piece s) ps = mintercalateMap s reverse xs === mintercalate s (map reverse xs)
      where
        xs = map unPiece ps

    unwordsMunwords :: [Piece] -> Property
    unwordsMunwords ps = munwords xs === unwords xs
      where
        xs = map unPiece ps

    associative :: Piece -> Piece -> Piece -> Property
    associative (Piece a) (Piece b) (Piece c) = (a <+> b) <+> c === a <+> (b <+> c)

    skipsEmpty :: Piece -> Piece -> Piece -> Property
    skipsEmpty (Piece s) (Piece a) (Piece b) = SkipEmpty.interpose s a b === intercalate s (filter (not . null) [a, b])

    skipsBlank :: Piece -> Piece -> Piece -> Property
    skipsBlank (Piece s) (Piece a) (Piece b) = SkipEmpty.interposeBy (all isSpace) s a b === intercalate s (filter (not . all isSpace) [a, b])

    associativeSkipEmpty :: Piece -> Piece -> Piece -> Property
    associativeSkipEmpty (Piece a) (Piece b) (Piece c) = (a SkipEmpty.<+?> b) SkipEmpty.<+?> c === a SkipEmpty.<+?> (b SkipEmpty.<+?> c)

    skipsEmptyElements :: Piece -> [Piece] -> Property
    skipsEmptyElements (Piece s) ps = SkipEmpty.mintercalate s xs === intercalate s (filter (not . null) xs)
      where
        xs = map unPiece ps

    skipsBlankElements :: Piece -> [Piece] -> Property
    skipsBlankElements (Piece s) ps = SkipEmpty.mintercalateBy (all isSpace) s xs === intercalate s (filter (not . all isSpace) xs)
      where
        xs = map unPiece ps

    foldsInterposeSkipEmpty :: Piece -> [Piece] -> Property
    foldsInterposeSkipEmpty (Piece s) ps = SkipEmpty.mintercalate s xs === foldr (SkipEmpty.interpose s) "" xs
      where
        xs = map unPiece ps

    foldsInterposeSkipEmptyBy :: Piece -> [Piece] -> Property
    foldsInterposeSkipEmptyBy (Piece s) ps = SkipEmpty.mintercalateBy (all isSpace) s xs === foldr (SkipEmpty.interposeBy (all isSpace) s) "" xs
      where
        xs = map unPiece ps

    unwordsSkipEmpty :: [Piece] -> Property
    unwordsSkipEmpty ps = SkipEmpty.munwords xs === unwords (filter (not . null) xs)
      where
        xs = map unPiece ps
