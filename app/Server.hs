{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString.UTF8 (toString)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Maybe (fromMaybe)
import Network.HTTP.Types (Header, status200, status204, status400, status404, status503, Query)
import Network.Wai.Handler.Warp (run)
import Network.Wai (Application, Response, rawPathInfo, responseLBS, queryString, Middleware, mapResponseHeaders)
import Text.Read (readMaybe)
import System.Timeout (timeout)

import Crossword (fromStrings)
import LanguageMachine (getCompletions', completeCrossword)
import WordLoader (loadData, FileResult)
import WordTrie (WordTrie(..), insertMany)

wordTrieFromFileResult :: FileResult -> WordTrie
wordTrieFromFileResult fr = insertMany WordTrie {nodes = []} $ snd fr

wordsApp :: [WordTrie] -> Application
wordsApp wts request respond = respond $ case rawPathInfo request of
    "/words" -> getWords wts (queryString request)
    "/solve" -> attemptCompletion wts (queryString request)
    _ -> notFound


attemptCompletion :: [WordTrie] -> Query -> Response
attemptCompletion wts q = case attemptCompletion_ wts q of
    Just cw -> responseLBS
        status200
        [("Content-Type", "text/plain")]
        (fromString cw)
    Nothing -> responseLBS
        status204
        [("Content-Type", "text/plain")]
        (fromString "No Solution Found")


attemptCompletion_ :: [WordTrie] -> Query -> Maybe String
attemptCompletion_ wts q = do
    cw <- lookup "crossword" q
    cwv <- cw
    let crossword = fromStrings $ lines $ toString cwv
    Just . show =<< completeCrossword crossword wts

findWordCompletions :: [WordTrie] -> Query -> Maybe String
findWordCompletions wts q = do
    wq <- lookup "word" q
    wqv <- wq
    let page = case lookup "page" q of
                Just pgv -> fromMaybe 0 $ readMaybe (maybe "0" toString pgv) :: Int
                Nothing -> 0
    let completions = getCompletions' wts (toString wqv)
    Just $ unwords $ take 10 $ drop (10 * page) completions

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

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"


withHeader :: Header -> Middleware
withHeader h app req respond = app req $ respond . addHeader h

addHeader :: Header -> Response -> Response
addHeader h = mapResponseHeaders (h :)

allowCors :: Middleware
allowCors = withHeader ("Access-Control-Allow-Origin", "http://localhost:8080")

timebound :: Middleware
timebound app req respond = do
    let sec = case lookup "timeout" $ queryString req of
                Just sv -> fromMaybe 30 $ readMaybe (maybe "0" toString sv) :: Int
                Nothing -> 30
    maybe (respond $ responseLBS status503 [("Content-Type", "text/plain")] "Timeout") pure
        =<< timeout (sec * 1000000) (app req respond)

port :: Int
port = 8081

main :: IO ()
main = do
    putStrLn "Loading Words..."
    (errors, results) <- loadData "data" []
    mapM_ print errors
    let wts = map wordTrieFromFileResult results
    putStrLn $ "Server Started at http://localhost:" ++ show port ++ "/"
    run port $ allowCors $ timebound $ wordsApp wts
