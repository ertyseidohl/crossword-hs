module DatabaseSpec (spec) where

import Test.Hspec

import Database (
    CrosswordData (..)
    , getConnection
    , closeConnection
    , createTable
    , createRecord
    , readRecord
    , updateRecord
    , testingonlyDropTable)

testName :: String
testName = "Test"

testData :: CrosswordData
testData = CrosswordData {
    name = testName
    , author = "Tester"
    , crossword = "abc\ndef\n...\n"
    , symmetry = "180"
    , customWords = "hello\nworld\n"}

testData2 :: CrosswordData
testData2 = CrosswordData {
    name = testName
    , author = "New Author"
    , crossword = "...\nabc\ndef\n"
    , symmetry = "90"
    , customWords = "world\nhello\n"}

spec :: Spec
spec =
    describe "Connects to a Database" $
        it "Creates a table and can write, update, and read crossword records" $ do
            conn <- getConnection "db/test.db"
            testingonlyDropTable conn
            createTable conn
            createRecord conn testData
            result <- readRecord conn testName
            result `shouldBe` Just testData
            updateRecord conn testData2
            result2 <- readRecord conn testName
            result2 `shouldBe` Just testData2
            closeConnection conn

        -- TODO clean up table once all done

