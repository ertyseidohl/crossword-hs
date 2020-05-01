import Test.Hspec

import Data.List (sort)

import LanguageMachine (getCompletions, completeCrossword)
import WordTrie (WordTrie(..), insertMany, insertMany1)
import Crossword (fromStrings)

emptyWordTrie :: WordTrie
emptyWordTrie = WordTrie {nodes = []}

dict :: [WordTrie]
dict = [
    insertMany1 emptyWordTrie ["cat", "hat", "bat"]
    , insertMany1 emptyWordTrie ["cat", "sat", "mat"]
    , insertMany1 emptyWordTrie ["cat", "that", "pat"]]

basicCrosswordDict :: [WordTrie]
basicCrosswordDict = [
    insertMany1 emptyWordTrie ["abc", "def", "ghi"]
    , insertMany1 emptyWordTrie ["adg", "beh", "cfi"]]

largerCrosswordDict :: [WordTrie]
largerCrosswordDict = [
    insertMany1 emptyWordTrie ["absdf", "abdah", "abiae", "absaf", "uvdhf", "bgskj", "klfds"]
    , insertMany1 emptyWordTrie ["abcde", "fghij", "klmno", "pqrst", "uvwxy"]
    , insertMany1 emptyWordTrie ["afkpu", "bglqv", "chmrw", "dinsx", "ejoty"]]

dictWithFreq :: [WordTrie]
dictWithFreq = [
    insertMany emptyWordTrie [("abcd", 10), ("efgh", 20)]
    , insertMany emptyWordTrie [("ghij", 15), ("klmn", 25)]]


main :: IO ()
main = hspec $ do
    describe "getCompletions" $ do
        it "Generates completions from several word tries" $
            sort (getCompletions dict ".at") `shouldBe` sort ["cat", "hat", "bat", "sat", "mat", "pat"]
        it "Returns just the word if the word has no wildcard chars" $
            getCompletions dict "cat" `shouldBe` ["cat"]
        it "Returns nothing if the string is not a word" $
            getCompletions dict "zat" `shouldBe` []
        it "Returns completions in order of priority, by word trie" $
            getCompletions dictWithFreq "...." `shouldBe` ["efgh", "abcd", "klmn", "ghij"]
    describe "completeCrossword" $ do
        it "Completes a crossword with no #s" $ do
            let crosswordStart = fromStrings ["ab.", "...", "..."]
            maybe "" show (completeCrossword crosswordStart basicCrosswordDict) `shouldBe` "abc\ndef\nghi\n"
        it "Completes a 5x5 empty crossword" $ do
            let crosswordStart = fromStrings ["ab...", ".....", ".....", ".....", "....."]
            maybe "" show (completeCrossword crosswordStart largerCrosswordDict) `shouldBe` "abcde\nfghij\nklmno\npqrst\nuvwxy\n"
