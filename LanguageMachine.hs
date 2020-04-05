module LanguageMachine (loadData) where

import System.Directory (listDirectory)
import Data.List ((\\), elemIndex, partition)
import Data.Maybe (mapMaybe, isNothing)
import Data.Functor ((<&>))
import Text.Read (readMaybe)
import Data.Either (isLeft, isRight, lefts, rights)

newtype LineResult' = LineResult' (String, Int)
newtype FileResult = FileResult (FilePath, [LineResult])
newtype FileResult' = FileResult' (FilePath, [LineResult'])
newtype FileErrors' = FileErrors' (FilePath, [String])

type LineResult = Either String (String, Int)

parseTsv :: String -> [LineResult]
parseTsv contents = map parseLine (lines contents)

parseLine :: String -> LineResult
parseLine line = do
    let i = elemIndex '\t' line
    case i of
        Just ind ->
            let (word, strCount) = splitAt ind line in
                let count = readMaybe strCount in
                    case count of
                        Just c -> (Right (word, c))
                        Nothing -> (Left ("Parse error (no number) on \"" ++ line ++ "\""))
        Nothing -> (Left ("Parse error (no tab char) on \"" ++ line ++ "\""))


partitionErrors :: [(FilePath, [LineResult])] -> ([FileErrors'], [FileResult'])
partitionErrors x = (map fst a, map snd a) where a = map partitionErrors' x

partitionErrors' :: (FilePath, [LineResult]) -> (FileErrors', FileResult')
partitionErrors' (fp, lrs) = (
        FileErrors' (fp, lefts lrs),
        FileResult' (fp, map LineResult' (rights lrs))
    )

displayAndDiscardErrors :: ([FileErrors'], [FileResult']) -> IO [FileResult']
displayAndDiscardErrors (es, rs) = do
    displayErrors es
    return rs

displayErrors :: [FileErrors'] -> IO ()
displayErrors [] = putStrLn "Done."
displayErrors (fe:fes) = do
    let fp = fst fe
    let ers = snd fe
    mapM_ putStrLn ers
    displayErrors fes

loadData :: FilePath -> [FilePath] -> IO [FileResult']
loadData path exclude = do
    files <- listDirectory path
    let filtered = files \\ exclude :: [FilePath]
    let prefixed = map ((path ++ "/") ++) filtered :: [FilePath]
    let allWords = traverse readFile prefixed <&> map parseTsv <&> zip filtered :: IO [(FilePath, [LineResult])]
    return $ displayAndDiscardErrors $ partitionErrors allWords

