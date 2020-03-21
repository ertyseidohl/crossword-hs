import Test.Hspec
import Control.Exception

import Crossword (fromStrings, getWordAt, Direction(ACROSS, DOWN))

main :: IO ()
main = hspec $ do
    describe "Import and show" $ do
        it "Imports and shows a 3x3 all-light crossword" $
            show (fromStrings ["abc", "def", "ghi"]) `shouldBe` "abc\ndef\nghi\n"
        it "Imports and shows a 1x3 all-light crossword" $
            show (fromStrings ["a", "d", "g"]) `shouldBe` "a\nd\ng\n"
        it "Imports and shows a 3x1 all-light crossword" $
            show (fromStrings ["abc"]) `shouldBe` "abc\n"
        it "Imports and shows a 0x0 crossword" $
            show (fromStrings []) `shouldBe` ""
        it "Imports and shows an all-dark crossword" $
            show (fromStrings ["###", "###", "###"]) `shouldBe` "###\n###\n###\n"
        it "Imports and shows a crossword with light and dark squares" $
            show (fromStrings ["ab#", "c#d", "#ef"]) `shouldBe` "ab#\nc#d\n#ef\n"
    describe "Validate" $
        it "Errors out on a non-rectangular crossword" $
            evaluate (fromStrings ["abc", "de", "fgh"]) `shouldThrow` errorCall "Input was not a rectangle"
    describe "Word At" $ do
        it "Finds an ACROSS word with no dark squares" $
            getWordAt (fromStrings ["abc", "def", "ghi"]) (1,1) ACROSS `shouldBe` "def"
        it "Finds a DOWN word with no dark squares" $
            getWordAt (fromStrings ["abc", "def", "ghi"]) (1,1) DOWN `shouldBe` "beh"
        it "Finds an ACROSS word with dark squares" $
            getWordAt (fromStrings ["abcd", "#fg#", "ijkl", "mnop"]) (1,1) ACROSS `shouldBe` "fg"
        it "Finds a DOWN word with dark squares" $
            getWordAt (fromStrings ["a#cd", "efgh", "ijkl", "m#op"]) (1,1) DOWN `shouldBe` "fj"
        it "Finds an ACROSS word with empty letters" $
            getWordAt (fromStrings ["abc", "...", "ghi"]) (1,1) ACROSS `shouldBe` "..."
        it "Finds a DOWN word with empty letters" $
            getWordAt (fromStrings ["a.c", "d.f", "g.i"]) (1,1) DOWN `shouldBe` "..."
