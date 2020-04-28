-- This is based on https://github.com/jaspervdj/websockets/blob/master/example/server.lhs

{-# LANGUAGE
    OverloadedStrings
    , DeriveGeneric
    , DeriveAnyClass #-}

module Main where

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import GHC.Generics

import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Network.WebSockets as WS

import Data.Aeson

data Request = Request {
    action :: Text
    , request :: Text
} deriving (Generic, Show, FromJSON)

data Response = Response {
    status :: Text
    , response :: Text
} deriving (Generic, Show, ToJSON)

type Client = (Text, WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn message
    forM_ clients $ \(_, conn) -> WS.sendTextData conn message

handleClientMessage :: WS.Connection -> MVar ServerState -> IO ()
handleClientMessage conn state = do
    msg <- WS.receiveData conn
    clients <- readMVar state
    let disconnect = do
        s <- modifyMVar state $ \s ->
          let s' = removeClient client s in return (s', s')
        broadcast (fst client <> " disconnected") s
    talk client state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (return ()) (handleClientMessage conn state)

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state = forever $ do
    msg <- WS.receiveData conn
    readMVar state >>= broadcast (user `mappend` ": " `mappend` msg)

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "127.0.0.1" 3339 $ application state
