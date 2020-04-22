import Test.Hspec
import Control.Exception
import WordTrie( WordTrie(..), isWord, insert, insertMany, getWords )

emptyWordTrie :: WordTrie
emptyWordTrie = WordTrie {nodes = []}

d :: WordTrie
d = insertMany emptyWordTrie ["hello", "world"]

main :: IO ()
main = hspec $ do
    describe "insert" $ do
        it "Inserts and finds a word" $
            isWord "hello" (insert emptyWordTrie "hello") `shouldBe` True
        it "Inserts and cannot find a different word" $
            isWord "bye" (insert emptyWordTrie "hello") `shouldBe` False
        it "Inserts and finds the first word" $
            isWord "hello" (insertMany emptyWordTrie ["hello", "bye"]) `shouldBe` True
        it "Inserts and finds the second word" $
            isWord "bye" (insertMany emptyWordTrie ["hello", "bye"]) `shouldBe` True
        it "Inserts and finds a branched word" $
            isWord "hero" (insertMany emptyWordTrie ["hello", "hero"]) `shouldBe` True
        it "Inserts and finds a subword" $
            isWord "hell" (insertMany emptyWordTrie ["hello", "hell"]) `shouldBe` True
        it "Cannot insert a dot character" $
            evaluate (insert emptyWordTrie "a.b") `shouldThrow` errorCall "Words cannot contain '.'"
        it "Cannot insert a hash character" $
            evaluate (insert emptyWordTrie "a#b") `shouldThrow` errorCall "Words cannot contain '#'"
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
            getWords ".at" (insertMany emptyWordTrie ["bat", "cat", "act"]) `shouldBe` ["cat", "bat"]
    describe "isWord" $ do
        it "Returns true if the string is a word" $
            isWord "hello" d `shouldBe` True
        it "Returns false if the string is not a word" $
            isWord "asdf" d `shouldBe` False
        it "Returns false if the string contains wildcards" $
            isWord "..llo" d `shouldBe` False
