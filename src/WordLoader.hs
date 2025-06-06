module WordLoader (loadData, FileErrors, FileResult) where

import System.Directory (listDirectory)
import System.IO (IOMode(ReadMode), hGetContents, hSetEncoding, utf8, openFile)
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

errorIfContainsIllegalChar :: String -> Int -> LineResult
errorIfContainsIllegalChar word c
    | ' ' `elem` word = Left ("Ignoring " ++ word ++ " because it contains ' '")
    | '#' `elem` word = Left ("Ignoring " ++ word ++ " because it contains '#'")
    | '.' `elem` word = Left ("Ignoring " ++ word ++ " because it contains '.'")
    | otherwise = Right (word, c)

parseLine :: String -> LineResult
parseLine line = do
    let i = elemIndex '\t' line
    case i of
        Just ind ->
            let (word, strCount) = splitAt ind line in
                let count = readMaybe strCount
                in case count of
                    Just c -> errorIfContainsIllegalChar word c
                    Nothing -> Left ("Parse error (no number) on \"" ++ line ++ "\"")
        Nothing -> Left ("Parse error (no tab char) on \"" ++ line ++ "\"")


partitionErrors :: [(FilePath, [LineResult])] -> ([FileErrors], [FileResult])
partitionErrors x = (map fst a, map snd a) where a = map partitionErrors' x

partitionErrors' :: (FilePath, [LineResult]) -> (FileErrors, FileResult)
partitionErrors' (fp, lrs) = (
        (fp, lefts lrs),
        (fp, rights lrs)
    )

readFileUtf8 :: FilePath -> IO String
readFileUtf8 fp = do
    inputHandle <- openFile fp ReadMode
    hSetEncoding inputHandle utf8
    hGetContents inputHandle

loadData :: FilePath -> [FilePath] -> IO ([FileErrors], [FileResult])
loadData filePath exclude = do
    files <- listDirectory filePath
    let filtered = files \\ exclude :: [FilePath]
    let prefixed = map ((filePath ++ "/") ++) filtered :: [FilePath]
    allWords <- traverse readFileUtf8 prefixed <&> zip filtered . map parseTsv
    return $ partitionErrors allWords
