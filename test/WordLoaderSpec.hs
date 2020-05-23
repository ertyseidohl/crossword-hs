module WordLoaderSpec (spec) where

import Test.Hspec

import WordLoader (loadData)

testDir :: String
testDir = "test_data"

spec :: Spec
spec = do
    describe "Load a directory" $ do
        it "Loads a directory full of valid data" $
            let d = loadData testDir ["bad_data.tsv", "bad_words.tsv"] in
                d `shouldReturn` ([("test1.tsv",[]),("test2.tsv",[])],[("test1.tsv",[("AREA",5492),("ERA",5212)]),("test2.tsv",[("ARIA",4308),("ORE",4208)])])
        it "Ignores and warns on invalid data" $
            let d = loadData testDir ["bad_words.tsv"] in
                d `shouldReturn` ([("test1.tsv",[]),("test2.tsv",[]),("bad_data.tsv",["Parse error (no number) on \"this\tis  bad\tdata\"","Parse error (no tab char) on \"reallybad\""])],[("test1.tsv",[("AREA",5492),("ERA",5212)]),("test2.tsv",[("ARIA",4308),("ORE",4208)]),("bad_data.tsv",[])])
        it "Ignores words containing invalid characters" $
            let d = loadData testDir ["test1.tsv", "test2.tsv", "bad_data.tsv"] in
                d `shouldReturn` ([("bad_words.tsv",["Ignoring ABC# because it contains '#'", "Ignoring ABC. because it contains '.'"])],[("bad_words.tsv",[])])
