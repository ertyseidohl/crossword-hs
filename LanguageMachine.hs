module LanguageMachine (loadData) where

import System.Directory (getDirectoryContents)
import Data.List ((\\), elemIndex)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)

defaultExcludes :: [FilePath]
defaultExcludes = [".", ".."]

parseTsv :: String -> [(String, Int)]
parseTsv contents = mapMaybe parseLine (lines contents)

parseLine :: String -> Maybe (String, Int)
parseLine line =
    case elemIndex '\t' line of
        Just i ->
            let
                (word, count) = splitAt i line
            in
                Just (word, read count :: Int)
        Nothing -> trace line Nothing

loadData :: FilePath -> [FilePath] -> IO [(String, [(String, Int)])]
loadData path exclude = do
    files <- getDirectoryContents path
    let filtered = files \\ (exclude ++ defaultExcludes)
    let prefixed = map ((path ++ "/") ++) filtered
    contents <- traverse readFile prefixed
    let results = map parseTsv contents
    return $ zip filtered results



