import Test.Hspec
import WordTrie( WordTrie(..), isWord, insert, insertMany )

emptyWordTrie :: WordTrie
emptyWordTrie = WordTrie {nodes = []}

main :: IO ()
main = hspec $
    describe "findWords" $ do
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

