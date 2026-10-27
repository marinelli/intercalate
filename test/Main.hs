{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.ByteString.Short qualified as SBS
import Data.Char
import Data.Intercalate qualified as C
import Data.Intercalate.SkipEmpty qualified as CS
import Data.List
import Data.List.NonEmpty qualified as NE
import Data.Monoid.Intercalate
import Data.Monoid.Intercalate.SkipEmpty
import Data.String
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
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
main =
  defaultMain
    ( testGroup
        "intercalate"
        [ examples
        , properties
        , testGroup
            "Data.Intercalate"
            [ fastJoin "String" id id
            , fastJoin "strict Text" T.pack id
            , fastJoin "lazy Text" TL.pack id
            , fastJoin "strict ByteString" B.pack id
            , fastJoin "lazy ByteString" BL.pack id
            , fastJoin "ShortByteString" (SBS.toShort . B.pack) id
            , fastJoin "Builder" BB.string7 BB.toLazyByteString
            ]
        , testGroup
            "Data.Intercalate.SkipEmpty"
            [ skipEmptyJoin "String" id
            , skipEmptyJoin "strict Text" T.pack
            , skipEmptyJoin "lazy Text" TL.pack
            , skipEmptyJoin "strict ByteString" B.pack
            , skipEmptyJoin "lazy ByteString" BL.pack
            , skipEmptyJoin "ShortByteString" (SBS.toShort . B.pack)
            ]
        ]
    )
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
        , example "interposeSkipEmpty" "left, right" (interposeSkipEmpty ", " "left" "right")
        , example "interposeSkipEmpty empty left" "right" (interposeSkipEmpty ", " "" "right")
        , example "interposeSkipEmpty empty right" "left" (interposeSkipEmpty ", " "left" "")
        , example "interposeSkipEmptyBy blank left" "right" (interposeSkipEmptyBy (all isSpace) ", " "  " "right")
        , example "<+?>" "foo bar" ("foo" <+?> "bar")
        , example "<+?> empty left" "bar" ("" <+?> "bar")
        , example "<+?> empty right" "foo" ("foo" <+?> "")
        , example "<+?> Text" "bar" (T.unpack (T.empty <+?> T.pack "bar"))
        , example "<+?> is lazy" "x y" (take 3 (foldr (<+?>) "" ("x" : "" : "y" : undefined)))
        , example "<+?> has the fixity of <>" $(reifyFixity '(<>) >>= stringE . show) $(reifyFixity '(<+?>) >>= stringE . show)
        , example "mintercalateSkipEmpty" "a, b" (mintercalateSkipEmpty ", " ["", "a", "", "b", ""])
        , example "mintercalateSkipEmpty only empty" "" (mintercalateSkipEmpty ", " ["", ""])
        , example "mintercalateSkipEmpty Text" "a, b" (T.unpack (mintercalateSkipEmpty (T.pack ", ") (map T.pack ["a", "", "b"])))
        , example "mintercalateSkipEmpty is lazy" "x y" (take 3 (mintercalateSkipEmpty " " ("x" : "" : "y" : undefined)))
        , example "mintercalateSkipEmptyBy" "a, b" (mintercalateSkipEmptyBy (all isSpace) ", " ["a", " ", "", "b"])
        , example "munwordsSkipEmpty" "foo bar" (munwordsSkipEmpty ["foo", "", "bar"])
        , example "Data.Intercalate.mintercalate ByteString" "a, b" (B.unpack (C.mintercalate (B.pack ", ") (map B.pack ["a", "b"])))
        , example "Data.Intercalate.mintercalate ShortByteString" "a, b" (B.unpack (SBS.fromShort (C.mintercalate (short ", ") (map short ["a", "b"]))))
        , example "Data.Intercalate.mintercalate Builder" "a, b" (BL.unpack (BB.toLazyByteString (C.mintercalate (BB.string7 ", ") (map BB.string7 ["a", "b"]))))
        , example "Data.Intercalate.mintercalate is lazy" "a" (take 1 (C.mintercalate "," ("a" : undefined)))
        , example "Data.Intercalate.munwords is lazy" "x y" (take 3 (C.munwords ("x" : "y" : undefined)))
        , example "Data.Intercalate.SkipEmpty.<+?> ByteString" "bar" (B.unpack (B.empty CS.<+?> B.pack "bar"))
        , example "Data.Intercalate.SkipEmpty.<+?> is lazy" "x y" (take 3 (foldr (CS.<+?>) "" ("x" : "" : "y" : undefined)))
        , example "Data.Intercalate.SkipEmpty.<+?> has the fixity of <>" $(reifyFixity '(<>) >>= stringE . show) $(reifyFixity '(CS.<+?>) >>= stringE . show)
        , example "Data.Intercalate.SkipEmpty.mintercalateSkipEmpty is lazy" "x y" (take 3 (CS.mintercalateSkipEmpty " " ("x" : "" : "y" : undefined)))
        ]
      where
        example :: String -> String -> String -> TestTree
        example name expected actual = testCase name (assertEqual "" expected actual)

        short :: String -> SBS.ShortByteString
        short = SBS.toShort . B.pack

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
    , testProperty "interposeSkipEmpty is intercalate without empty operands" skipsEmpty
    , testProperty "interposeSkipEmptyBy is intercalate without skipped operands" skipsBlank
    , testProperty "<+?> is associative" associativeSkipEmpty
    , testProperty "mintercalateSkipEmpty is intercalate without empty elements" skipsEmptyElements
    , testProperty "mintercalateSkipEmptyBy is intercalate without skipped elements" skipsBlankElements
    , testProperty "mintercalateSkipEmpty is a right fold of interposeSkipEmpty" foldsInterposeSkipEmpty
    , testProperty "mintercalateSkipEmptyBy is a right fold of interposeSkipEmptyBy" foldsInterposeSkipEmptyBy
    , testProperty "munwordsSkipEmpty is unwords without empty elements" unwordsSkipEmpty
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
    skipsEmpty (Piece s) (Piece a) (Piece b) = interposeSkipEmpty s a b === intercalate s (filter (not . null) [a, b])

    skipsBlank :: Piece -> Piece -> Piece -> Property
    skipsBlank (Piece s) (Piece a) (Piece b) = interposeSkipEmptyBy (all isSpace) s a b === intercalate s (filter (not . all isSpace) [a, b])

    associativeSkipEmpty :: Piece -> Piece -> Piece -> Property
    associativeSkipEmpty (Piece a) (Piece b) (Piece c) = (a <+?> b) <+?> c === a <+?> (b <+?> c)

    skipsEmptyElements :: Piece -> [Piece] -> Property
    skipsEmptyElements (Piece s) ps = mintercalateSkipEmpty s xs === intercalate s (filter (not . null) xs)
      where
        xs = map unPiece ps

    skipsBlankElements :: Piece -> [Piece] -> Property
    skipsBlankElements (Piece s) ps = mintercalateSkipEmptyBy (all isSpace) s xs === intercalate s (filter (not . all isSpace) xs)
      where
        xs = map unPiece ps

    foldsInterposeSkipEmpty :: Piece -> [Piece] -> Property
    foldsInterposeSkipEmpty (Piece s) ps = mintercalateSkipEmpty s xs === foldr (interposeSkipEmpty s) "" xs
      where
        xs = map unPiece ps

    foldsInterposeSkipEmptyBy :: Piece -> [Piece] -> Property
    foldsInterposeSkipEmptyBy (Piece s) ps = mintercalateSkipEmptyBy (all isSpace) s xs === foldr (interposeSkipEmptyBy (all isSpace) s) "" xs
      where
        xs = map unPiece ps

    unwordsSkipEmpty :: [Piece] -> Property
    unwordsSkipEmpty ps = munwordsSkipEmpty xs === unwords (filter (not . null) xs)
      where
        xs = map unPiece ps

-- | Checks that the functions of "Data.Intercalate" give the results of
-- their twins in "Data.Monoid.Intercalate", after @observe@.
fastJoin :: (C.Intercalate a, IsString a, Eq o, Show o) => String -> (String -> a) -> (a -> o) -> TestTree
fastJoin name pack observe =
  testGroup
    name
    [ testProperty "mintercalate is the core one" joins
    , testProperty "mintercalateMap is the core one" mapsAndJoins
    , testProperty "munwords is the core one" unwordsJoins
    ]
  where
    joins :: Piece -> [Piece] -> Property
    joins (Piece s) ps = observe (C.mintercalate (pack s) xs) === observe (mintercalate (pack s) xs)
      where
        xs = map (pack . unPiece) ps

    mapsAndJoins :: Piece -> [Piece] -> Property
    mapsAndJoins (Piece s) ps = observe (C.mintercalateMap (pack s) pack xs) === observe (mintercalateMap (pack s) pack xs)
      where
        xs = map unPiece ps

    unwordsJoins :: [Piece] -> Property
    unwordsJoins ps = observe (C.munwords xs) === observe (munwords xs)
      where
        xs = map (pack . unPiece) ps

-- | Checks the 'CS.IsEmpty' law, and that the functions of
-- "Data.Intercalate.SkipEmpty" give the results of their twins in
-- "Data.Monoid.Intercalate.SkipEmpty".
skipEmptyJoin :: (C.Intercalate a, CS.IsEmpty a, IsString a, Eq a, Show a) => String -> (String -> a) -> TestTree
skipEmptyJoin name pack =
  testGroup
    name
    [ testProperty "isEmpty is == mempty" isEmptyLaw
    , testProperty "interposeSkipEmpty is the core one" interposes
    , testProperty "<+?> is the core one" spaces
    , testProperty "mintercalateSkipEmpty is the core one" joins
    , testProperty "mintercalateSkipEmptyBy is the core one" joinsBy
    , testProperty "munwordsSkipEmpty is the core one" unwordsJoins
    ]
  where
    isEmptyLaw :: Piece -> Property
    isEmptyLaw (Piece x) = CS.isEmpty (pack x) === (pack x == mempty)

    interposes :: Piece -> Piece -> Piece -> Property
    interposes (Piece s) (Piece a) (Piece b) = CS.interposeSkipEmpty (pack s) (pack a) (pack b) === interposeSkipEmpty (pack s) (pack a) (pack b)

    spaces :: Piece -> Piece -> Property
    spaces (Piece a) (Piece b) = (pack a CS.<+?> pack b) === (pack a <+?> pack b)

    joins :: Piece -> [Piece] -> Property
    joins (Piece s) ps = CS.mintercalateSkipEmpty (pack s) xs === mintercalateSkipEmpty (pack s) xs
      where
        xs = map (pack . unPiece) ps

    joinsBy :: Piece -> [Piece] -> Property
    joinsBy (Piece s) ps = CS.mintercalateSkipEmptyBy (== pack " ") (pack s) xs === mintercalateSkipEmptyBy (== pack " ") (pack s) xs
      where
        xs = map (pack . unPiece) ps

    unwordsJoins :: [Piece] -> Property
    unwordsJoins ps = CS.munwordsSkipEmpty xs === munwordsSkipEmpty xs
      where
        xs = map (pack . unPiece) ps
