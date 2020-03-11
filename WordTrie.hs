module WordTrie (WordTrie(..), insert, insertMany, isWord, getWords) where

import Data.List (find, findIndex)

newtype WordTrie = WordTrie {nodes :: [LetterNode]}

data LetterNode = LetterNode {char :: Char, children :: [LetterNode], endsWord :: Bool, parent :: Maybe LetterNode}

instance Show WordTrie where
    show w = unwords $ map show (nodes w)

instance Show LetterNode where
    show l = show (char l) ++ if endsWord l then "*" else "" ++ unwords (map show (children l))

insert :: WordTrie -> String -> WordTrie
insert w [] = w
insert w str =
    let ns = insertLetterNode (nodes w) str Nothing
    in WordTrie {
        nodes = ns
    }

insertMany :: WordTrie -> [String] -> WordTrie
insertMany = foldl insert

insertLetterNode :: [LetterNode] -> String -> Maybe LetterNode -> [LetterNode]
insertLetterNode ls [] _ = ls
insertLetterNode ls (c:cs) p =
    case findLetterIndex ls c of
        Just i ->
            let
                (before, n:after) = splitAt i ls
                newNode = LetterNode {
                    char = c
                    , children = insertLetterNode (children n) cs (Just newNode)
                    , endsWord = endsWord n || null cs
                    , parent = p

                }
            in
                before ++ newNode : after
        Nothing ->
            let newNode = LetterNode {
                char = c
                , children = insertLetterNode [] cs (Just newNode)
                , endsWord = null cs
                , parent = p
            }
            in newNode : ls

findLetterIndex :: [LetterNode] -> Char -> Maybe Int
findLetterIndex ns c = findIndex (\n -> char n == c) ns

findLetter :: [LetterNode] -> Char -> Maybe LetterNode
findLetter ns c = find (\n -> char n == c) ns

isWord :: WordTrie -> String -> Bool
isWord _ [] = False
isWord w [c] = maybe False endsWord (findLetter (nodes w) c)
isWord w (c:cs) = case findLetter (nodes w) c of
    Just n -> isWord_ n cs
    Nothing -> False

isWord_ :: LetterNode -> String -> Bool
isWord_ l [] = endsWord l
isWord_ l [c] = case findLetter (children l) c of
    Just n -> isWord_ n []
    Nothing -> False
isWord_ l (c:cs) = case findLetter (children l) c of
    Just n -> isWord_ n cs
    Nothing -> False

getWords :: WordTrie -> String -> [String]
getWords _ [] = []
getWords w ('.':cs) =
    let leaves = concatMap (getLeaves cs) (nodes w)
    in produceWords leaves
getWords w (c:cs) = case findLetter (nodes w) c of
    Just n -> produceWords $ getLeaves cs n
    Nothing -> []

produceWords :: [LetterNode] -> [String]
produceWords = map (reverse . readUp)

readUp :: LetterNode -> String
readUp l = case parent l of
    Just p -> char l : readUp p
    Nothing -> [char l]

getLeaves :: String -> LetterNode -> [LetterNode]
getLeaves [] l = [l | endsWord l]
getLeaves ('.':cs) l =
    concatMap (getLeaves cs) (children l)
getLeaves (c:cs) l = case findLetter (children l) c of
    Just n -> getLeaves cs n
    Nothing -> []

