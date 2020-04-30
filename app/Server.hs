{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Lazy.UTF8 (toString, fromString)
import Network.HTTP.Types (status200, status400, status404, Query)
import Network.Wai.Handler.Warp (run)
import Network.Wai (Application, Response, rawPathInfo, responseFile, responseLBS, queryString)


app :: Application
app request respond = respond $ case rawPathInfo request of
    "/" -> index
    "/words" -> getWords (queryString request)
    _ -> notFound

findWordCompletions :: Query -> Maybe String
findWordCompletions q =
    case lookup "word" q of
        Just wq -> case wq of
            Just wqv -> Just $ toString (fromStrict wqv)
            Nothing -> Nothing
        Nothing -> Nothing

getWords :: Query -> Response
getWords q =
    case findWordCompletions q of
        Just wcs -> responseLBS
            status200
            [("Content-Type", "application/json")]
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
    run 8080 app
