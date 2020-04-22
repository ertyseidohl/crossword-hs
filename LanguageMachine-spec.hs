import Test.Hspec

import Data.List (sort)

import LanguageMachine (getCompletions, completeCrossword)
import WordTrie (WordTrie(..), insertMany)
import Crossword (fromStrings)

emptyWordTrie :: WordTrie
emptyWordTrie = WordTrie {nodes = []}

dict :: [WordTrie]
dict = [
    insertMany emptyWordTrie ["cat", "hat", "bat"]
    , insertMany emptyWordTrie ["cat", "sat", "mat"]
    , insertMany emptyWordTrie ["cat", "that", "pat"]]

basicCrosswordDict :: [WordTrie]
basicCrosswordDict = [
    insertMany emptyWordTrie ["abc", "def", "ghi"]
    , insertMany emptyWordTrie ["adg", "beh", "cfi"]]

largerCrosswordDict :: [WordTrie]
largerCrosswordDict = [
    insertMany emptyWordTrie ["absdf", "abdah", "abiae", "absaf", "uvdhf", "bgskj", "klfds"]
    , insertMany emptyWordTrie ["abcde", "fghij", "klmno", "pqrst", "uvwxy"]
    , insertMany emptyWordTrie ["afkpu", "bglqv", "chmrw", "dinsx", "ejoty"]]


main :: IO ()
main = hspec $ do
    describe "getCompletions" $ do
        it "Generates completions from several word tries" $
            sort (getCompletions dict ".at") `shouldBe` sort ["cat", "hat", "bat", "sat", "mat", "pat"]
        it "Returns just the word if the word has no wildcard chars" $
            getCompletions dict "cat" `shouldBe` ["cat"]
        it "Returns nothing if the string is not a word" $
            getCompletions dict "zat" `shouldBe` []
    describe "completeCrossword" $ do
        it "Completes a crossword with no #s" $ do
            let crosswordStart = fromStrings ["ab.", "...", "..."]
            maybe "" show (completeCrossword crosswordStart basicCrosswordDict) `shouldBe` "abc\ndef\nghi\n"
        it "Completes a 5x5 empty crossword" $ do
            let crosswordStart = fromStrings ["ab...", ".....", ".....", ".....", "....."]
            maybe "" show (completeCrossword crosswordStart largerCrosswordDict) `shouldBe` "abcde\nfghij\nklmno\npqrst\nuvwxy\n"
