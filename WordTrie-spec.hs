import Test.Hspec
import WordTrie( WordTrie(..), isWord, insert, insertMany, getWords )

emptyWordTrie :: WordTrie
emptyWordTrie = WordTrie {nodes = []}

d :: WordTrie
d = insertMany emptyWordTrie ["hello", "world"]

main :: IO ()
main = hspec $ do
    describe "insert" $ do
        it "Inserts and finds a word" $
            isWord (insert emptyWordTrie "hello") "hello" `shouldBe` True
        it "Inserts and cannot find a different word" $
            isWord (insert emptyWordTrie "hello") "bye" `shouldBe` False
        it "Inserts and finds the first word" $
            isWord (insertMany emptyWordTrie ["hello", "bye"]) "hello" `shouldBe` True
        it "Inserts and finds the second word" $
            isWord (insertMany emptyWordTrie ["hello", "bye"]) "bye" `shouldBe` True
        it "Inserts and finds a branched word" $
            isWord (insertMany emptyWordTrie ["hello", "hero"]) "hero" `shouldBe` True
        it "Inserts and finds a subword" $
            isWord (insertMany emptyWordTrie ["hello", "hell"]) "hell" `shouldBe` True
    describe "find" $ do
        it "Handles missing letters at the start" $
            getWords d "..llo" `shouldBe` ["hello"]
        it "Handles missing letters at the end" $
            getWords d "hel.." `shouldBe` ["hello"]
        it "Handles missing letters in the middle" $
            getWords d "he..o" `shouldBe` ["hello"]
        it "Handles only missing letters" $
            getWords d "....." `shouldBe` ["world", "hello"]
        it "Handles words that differ by one letter" $
            getWords (insertMany emptyWordTrie ["bat", "cat", "act"]) ".at" `shouldBe` ["cat", "bat"]
