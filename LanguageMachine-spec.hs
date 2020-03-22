import Test.Hspec

import LanguageMachine (loadData)

badData :: [FilePath]
badData = ["bad_data.tsv"]

testDir :: String
testDir = "test_data"

main :: IO ()
main = hspec $
    describe "Load a directory" $
        it "Loads a directory full of valid data" $ do
            let d = loadData testDir badData
            d `shouldReturn` [
                ("test1.tsv", [("AREA", 5492), ("ERA", 5212)])
                , ("test2.tsv", [("ARIA", 4308), ("ORE", 4208)])]

