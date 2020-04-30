import Test.Hspec
import Control.Exception
import WordTrie( WordTrie(..), isWord, insert, insertMany, insertMany1, getWords, getWordFrequency )

emptyWordTrie :: WordTrie
emptyWordTrie = WordTrie {nodes = []}

d :: WordTrie
d = insertMany1 emptyWordTrie ["hello", "world"]

main :: IO ()
main = hspec $ do
    describe "insert" $ do
        it "Inserts and finds a word" $
            getWordFrequency "hello" (insert emptyWordTrie ("hello", 1)) `shouldBe` Just 1
        it "Inserts and cannot find a different word" $
            getWordFrequency "bye" (insert emptyWordTrie ("hello", 1)) `shouldBe` Nothing
        it "Inserts and finds the first word" $
            getWordFrequency "hello" (insertMany emptyWordTrie [("hello", 1), ("bye", 2)]) `shouldBe` Just 1
        it "Inserts and finds the second word" $
            getWordFrequency "bye" (insertMany emptyWordTrie [("hello", 1), ("bye", 2)]) `shouldBe` Just 2
        it "Inserts and finds a branched word" $
            getWordFrequency "hero" (insertMany emptyWordTrie [("hello", 1), ("hero", 2)]) `shouldBe` Just 2
        it "Inserts and finds a subword" $
            getWordFrequency "hell" (insertMany emptyWordTrie [("hello", 1), ("hell", 2)]) `shouldBe` Just 2
        it "Cannot insert a dot character" $
            evaluate (insert emptyWordTrie ("a.b", 1)) `shouldThrow` errorCall "Words cannot contain '.'"
        it "Cannot insert a hash character" $
            evaluate (insert emptyWordTrie ("a#b", 1)) `shouldThrow` errorCall "Words cannot contain '#'"
        it "Requires a non-0 frequency" $
            evaluate (insert emptyWordTrie ("abc", 0)) `shouldThrow` errorCall "Words cannot have a frequency <= 0"
        it "Requires a >0 frequency" $
            evaluate (insert emptyWordTrie ("abc", -1)) `shouldThrow` errorCall "Words cannot have a frequency <= 0"
    describe "find" $ do
        it "Handles missing letters at the start" $
            getWords "..llo" d `shouldBe` ["hello"]
        it "Handles missing letters at the end" $
            getWords "hel.." d `shouldBe` ["hello"]
        it "Handles missing letters in the middle" $
            getWords "he..o" d `shouldBe` ["hello"]
        it "Handles only missing letters" $
            getWords "....." d `shouldBe` ["world", "hello"]
        it "Handles words that differ by one letter" $
            getWords ".at" (insertMany1 emptyWordTrie ["bat", "cat", "act"]) `shouldBe` ["cat", "bat"]
    describe "findByFrequency" $
        it "Sorts found words by frequency" $
            getWords ".at" (insertMany emptyWordTrie [("sat", 1000), ("bat", 1), ("cat", 3), ("hat", 2)]) `shouldBe` ["sat", "cat", "hat", "bat"]
    describe "isWord" $ do
        it "Returns true if the string is a word" $
            isWord "hello" d `shouldBe` True
        it "Returns false if the string is not a word" $
            isWord "asdf" d `shouldBe` False
        it "Returns false if the string contains wildcards" $
            isWord "..llo" d `shouldBe` False
