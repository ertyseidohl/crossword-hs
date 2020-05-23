
module CrosswordSpec (spec) where

import Test.Hspec
import Control.Exception

import Crossword (
    fromStrings,
    getWordAt,
    getWordAtStartSquare,
    Direction(ACROSS, DOWN),
    getStartSquares,
    addWordAt)

spec :: Spec
spec = do
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
            -- We attempt to print the crossword to force evaluation.
            -- For some reason `evaluate` didn't work.
            print (fromStrings ["abc", "de", "fgh"]) `shouldThrow` errorCall "Input was not a rectangle"
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
    describe "Word At Start Square" $ do
        it "Finds an ACROSS word with no dark squares" $
            getWordAtStartSquare (fromStrings ["abc", "def", "ghi"]) ((1,1), ACROSS) `shouldBe` "ef"
        it "Finds a DOWN word with no dark squares" $
            getWordAtStartSquare (fromStrings ["abc", "def", "ghi"]) ((1,1), DOWN) `shouldBe` "eh"
        it "Finds a DOWN word with dark squares" $
            getWordAtStartSquare (fromStrings ["a#cd", "efgh", "ijkl", "m#op"]) ((1,1), DOWN) `shouldBe` "fj"
        it "Finds an ACROSS word with empty letters" $
            getWordAtStartSquare (fromStrings ["abc", "...", "ghi"]) ((1,1), ACROSS) `shouldBe` ".."
    describe "Get Clues" $ do
        it "Finds all clues in a crossword with no dark squares" $
            getStartSquares (fromStrings ["abc", "def", "ghi"]) `shouldBe` [((0,0),ACROSS),((0,0),DOWN),((1,0),DOWN),((2,0),DOWN),((0,1),ACROSS),((0,2),ACROSS)]
        it "Finds all clues in a crossword with dark squares" $
            getStartSquares (fromStrings ["abc", "d#f", "ghi"]) `shouldBe` [((0,0),ACROSS),((0,0),DOWN),((1,0),DOWN),((2,0),DOWN),((0,1),ACROSS),((2,1),ACROSS),((0,2),ACROSS),((1,2),DOWN)]
    describe "Add Word" $ do
        it "Adds a word to a crossword ACROSS" $
            show (addWordAt (fromStrings ["...", "...", "..."]) (0, 0) ACROSS "abc") `shouldBe` "abc\n...\n...\n"
        it "Adds a word to a crossword DOWN" $
            show (addWordAt (fromStrings ["...", "...", "..."]) (0, 0) DOWN "abc") `shouldBe` "a..\nb..\nc..\n"
        it "Throws an error if we try to add a word that doesn't fit ACROSS" $
            evaluate (show (addWordAt (fromStrings ["...", "...", "..."]) (0, 0) ACROSS "abcd")) `shouldThrow` errorCall "Out of bounds coord in addLetterAt"
        it "Throws an error if we try to add a word that doesn't fit DOWN" $
            evaluate (show (addWordAt (fromStrings ["...", "...", "..."]) (0, 0) DOWN "abcd")) `shouldThrow` errorCall "Out of bounds coord in addLetterAt"
        it "Throws an error if we try to add a word that overlaps a dark square ACROSS" $
            evaluate (show (addWordAt (fromStrings ["..#", "...", "..."]) (0, 0) ACROSS "abc")) `shouldThrow` errorCall "Added word would overlap dark square"
        it "Throws an error if we try to add a word that overlaps a dark square DOWN" $
            evaluate (show (addWordAt (fromStrings ["...", "...", "#.."]) (0, 0) DOWN "abc")) `shouldThrow` errorCall "Added word would overlap dark square"
