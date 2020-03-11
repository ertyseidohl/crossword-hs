module WordTrie (WordTrie(..), insert, insertMany, isWord) where

import Data.List (findIndex)

data WordTrie = EmptyWordTrie | WordTrie {char :: Char, endsWord :: Bool, children :: [WordTrie]}

instance Show WordTrie where
    show EmptyWordTrie = "/"
    show w =  show (char w) ++ if endsWord w then "*" else "" ++ unwords (map show (children w))

insert :: WordTrie -> String -> WordTrie
insert EmptyWordTrie [] = EmptyWordTrie
insert EmptyWordTrie [c] =
    WordTrie {
    char = c
    , endsWord = True
    , children = []
}
insert EmptyWordTrie (c:cs) =
    WordTrie {
        char = c
        , endsWord = False
        , children = [insert EmptyWordTrie cs]
    }
insert w [] = w
insert w [c] = WordTrie {char = c, endsWord = True, children = children w}
insert w (c:cs) = WordTrie {
    char = char w
    , endsWord = endsWord w
    , children =
        case findChildIndex w c of
            Just i -> let (xs,child:ys) = splitAt i (children w) in
                xs ++ insert child cs : ys
            Nothing -> insert EmptyWordTrie cs : children w

    }

findChildIndex :: WordTrie -> Char -> Maybe Int
findChildIndex w c = findIndex (\wt -> char wt == c) (children w)

insertMany :: WordTrie -> [String] -> WordTrie
insertMany = foldl insert

getChild :: [WordTrie] -> Char -> Maybe WordTrie
getChild [] _ = Nothing
getChild (w:ws) c
    | char w == c = Just w
    | otherwise = getChild ws c

isWord :: WordTrie -> String -> Bool
isWord _ [] = error "Tried to read letter from empty trie"
isWord w [c]
    | char w == c = endsWord w
    | otherwise = False
isWord w (c:cs)
    | char w == c = case getChild (children w) (head cs) of
        Just child -> isWord child cs
        Nothing -> False
    | otherwise = False


    --


insert :: WordTrie -> String -> WordTrie
insert w [] = w
insert w (str@s:_) =
    let
        nodes = insertLetterNode (nodes w) s
    in
        WordTrie {
            nodes = nodes
        }

insertLetterNode :: [LetterNode] -> String -> [LetterNode]
insertLetterNode lns (c:cs) =
    case findLetterIndex lns c of
        Just i ->
            let
                (before, n:after) = splitAt i lns
            in
                before ++ insertTrie n str : after
        Nothing ->
            lns : insertTrie n str

findLetterIndex :: [LetterNode] -> Char -> Maybe Int
findLetterIndex ns c = findIndex (\n -> char n == c) (ns)

insertTrie :: LetterNode -> String -> LetterNode
insertTrie ln [] = ln
insertTrie ln [c] = LetterNode {
    char = c
    , children = children ln
    , endsWord = True
}
insertTrie ln (s:ss) = LetterNode {
    char = c
    , children = insertLetterNode (children ln) s
    , endsWord = endsWord ln
}
