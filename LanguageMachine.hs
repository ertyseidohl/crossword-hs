module LanguageMachine (loadData) where

import System.Directory (listDirectory)
import Data.List ((\\), elemIndex)
import Data.Maybe (mapMaybe)
import Data.Functor ((<&>))
import Text.Read (readMaybe)

parseTsv :: String -> [(String, Int)]
parseTsv contents = mapMaybe parseLine (lines contents)

parseLine :: String -> Maybe (String, Int)
parseLine line = do
    i <- elemIndex '\t' line
    let (word, strCount) = splitAt i line
    count <- readMaybe strCount
    return (word, count)

loadData :: FilePath -> [FilePath] -> IO [(String, [(String, Int)])]
loadData path exclude = do
    files <- listDirectory path
    let filtered = files \\ exclude
    let prefixed = map ((path ++ "/") ++) filtered
    traverse readFile prefixed
        <&> map parseTsv
        <&> zip filtered
