module FileLoader (loadData) where

import System.Directory (listDirectory)
import Data.List ((\\), elemIndex)
import Data.Functor ((<&>))
import Text.Read (readMaybe)
import Data.Either (lefts, rights)

type FileErrors = (String, [String])
type LineResult' = (String, Int)
type LineResult = Either String LineResult'
type FileResult = (FilePath, [LineResult'])

parseTsv :: String -> [LineResult]
parseTsv contents = map parseLine (lines contents)

parseLine :: String -> LineResult
parseLine line = do
    let i = elemIndex '\t' line
    case i of
        Just ind ->
            let (word, strCount) = splitAt ind line in
                let count = readMaybe strCount
                in case count of
                    Just c -> Right (word, c)
                    Nothing -> Left ("Parse error (no number) on \"" ++ line ++ "\"")
        Nothing -> Left ("Parse error (no tab char) on \"" ++ line ++ "\"")


partitionErrors :: [(FilePath, [LineResult])] -> ([FileErrors], [FileResult])
partitionErrors x = (map fst a, map snd a) where a = map partitionErrors' x

partitionErrors' :: (FilePath, [LineResult]) -> (FileErrors, FileResult)
partitionErrors' (fp, lrs) = (
        (fp, lefts lrs),
        (fp, rights lrs)
    )

loadData :: FilePath -> [FilePath] -> IO ([FileErrors], [FileResult])
loadData filePath exclude = do
    files <- listDirectory filePath
    let filtered = files \\ exclude :: [FilePath]
    let prefixed = map ((filePath ++ "/") ++) filtered :: [FilePath]
    allWords <- traverse readFile prefixed <&> map parseTsv
                                <&> zip filtered
    return $ partitionErrors allWords



