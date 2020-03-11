import Test.Hspec
import LanguageMachine (findWords)
import WordList (WordList)

main :: IO ()
main = hspec $ do
    describe "findWords" $ do
        it "Handles missing letters at the start" $
            findWords d "..llo" `shouldBe` "hello"
        it "Handles missing letters at the end"
            findWords d "hel.." `shouldBe` "hello"
        it "Handles missing letters in the middle"
            findWords d "he..o" `shouldBe` "hello"

d :: WordList
d = ["hello", "world"]
