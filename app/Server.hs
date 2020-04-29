{-# LANGUAGE OverloadedStrings, PackageImports #-}
import Network.HTTP.Types (status200, status404, Query)
import Network.Wai.Handler.Warp (run)
import Network.Wai (Application, Response, rawPathInfo, responseFile, responseLBS, requestBody)


app :: Application
app request respond = respond $ case rawPathInfo request of
    "/" -> index
    _ -> notFound

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
    putStrLn $ "http://localhost:8080/"
    run 8080 app
