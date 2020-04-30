{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString.UTF8 (toString)
import Data.ByteString.Lazy.UTF8 (fromString)
import Network.HTTP.Types (status200, status400, status404, Query)
import Network.Wai.Handler.Warp (run)
import Network.Wai (Application, Response, rawPathInfo, responseFile, responseLBS, queryString)

import FileLoader (loadData, FileResult)
import WordTrie (WordTrie(..), insertMany)
import LanguageMachine (getCompletions)

wordTrieFromFileResult :: FileResult -> WordTrie
wordTrieFromFileResult fr = insertMany WordTrie {nodes = []} $ snd fr

app :: [WordTrie] -> Application
app wts request respond = respond $ case rawPathInfo request of
    "/" -> index
    "/words" -> getWords wts (queryString request)
    _ -> notFound

findWordCompletions :: [WordTrie] -> Query -> Maybe String
findWordCompletions wts q = do
    wq <- lookup "word" q
    wqv <- wq
    Just (
        let completions = getCompletions wts $ toString wqv
        in unwords completions)

getWords :: [WordTrie] -> Query -> Response
getWords wts q =
    case findWordCompletions wts q of
        Just wcs -> responseLBS
            status200
            [("Content-Type", "text/plain")]
            (fromString wcs)
        Nothing -> responseLBS
            status400
            [("Content-Type", "text/plain")]
            "Need to provide 'word' query param"

index :: Response
index = responseFile
    status200
    [("Content-Type", "text/html")]
    "server/index.html"
    Nothing

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"

main :: IO ()
main = do
    putStrLn "http://localhost:8080/"
    putStrLn "Loading Words"
    (errors, results) <- loadData "data" []
    mapM_ print errors
    let wts = map wordTrieFromFileResult results
    putStrLn "Loaded Words!"
    run 8080 (app wts)
