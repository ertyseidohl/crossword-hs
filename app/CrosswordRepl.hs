import FileLoader (loadData, FileResult)
import WordTrie (WordTrie(..), insertMany)
import LanguageMachine (completeCrossword)
import Crossword (fromStrings)

wordTrieFromFileResult :: FileResult -> WordTrie
wordTrieFromFileResult fr = insertMany WordTrie {nodes = []} $ snd fr

main :: IO ()
main = do
    putStrLn "Loading Words"
    (errors, results) <- loadData "data" []
    mapM_ print errors
    let wts = map wordTrieFromFileResult results
    putStrLn "Loaded Words!"
    getUserInput wts []

getUserInput :: [WordTrie] -> [String] -> IO ()
getUserInput wts curr = do
    line <- getLine
    if line == "" then do
        print "Completing..."
        case completeCrossword (fromStrings $ reverse curr) wts of
            Just cw -> do
                print cw
                getUserInput wts []
            Nothing -> do
                print "Found no valid completion."
                getUserInput wts []
    else if head line == '-' then do
        print $ "Removing " ++ tail line
        getUserInput wts []
    else
        getUserInput wts (line : curr)
