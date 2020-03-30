module LanguageMachine (loadData) where

import System.Directory (listDirectory)
import Data.List ((\\), elemIndex, partition)
import Data.Maybe (mapMaybe, isNothing)
import Data.Functor ((<&>))
import Text.Read (readMaybe)

parseTsv :: String -> [Maybe (String, Int)]
parseTsv contents = map parseLine (lines contents)

parseLine :: String -> Maybe (String, Int)
parseLine line = do
    i <- elemIndex '\t' line
    let (word, strCount) = splitAt i line
    count <- readMaybe strCount
    case count of
        Just c -> Just (word, c)
        Nothing -> Nothing

loadData :: FilePath -> [FilePath] -> IO [(String, [(String, Int)])]
loadData path exclude = do
    files <- listDirectory path
    let filtered = files \\ exclude
    let prefixed = map ((path ++ "/") ++) filtered
    let allWords = traverse readFile prefixed <&> map parseTsv <&> zip filtered
    let (errors, wordData) = partition (isNothing . fst) allWords
    mapM_ (putStrLn . ("Read error at" ++) . show) errors
    return mapMaybe (\x -> x) wordData


