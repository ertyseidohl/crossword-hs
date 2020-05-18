module WordTrie (
    WordTrie(..),
    insert,
    insert1,
    insertMany,
    insertMany1,
    getWordFrequency,
    isWord,
    getWords)
where

import Data.Function (on)
import Data.List (find,findIndex, foldl', sortBy)
import Data.Maybe (isJust, fromMaybe)

newtype WordTrie = WordTrie {nodes :: [LetterNode]}

data LetterNode = LetterNode {char :: Char, children :: [LetterNode], wordFrequency :: Maybe Int, parent :: Maybe LetterNode}

type WordWithFreq = (String, Int)

instance Show WordTrie where
    show w = unwords $ map show (nodes w)

instance Show LetterNode where
    show l = show (char l) ++ if isJust (wordFrequency l) then "*" else "" ++ unwords (map show (children l))

insert1 :: WordTrie -> String -> WordTrie
insert1 w s = insert w (s, 1)

insert :: WordTrie -> WordWithFreq -> WordTrie
insert w ([], _) = w
insert w wwf@(str, freq)
    | freq <= 0 = error "Words cannot have a frequency <= 0"
    | '.' `elem` str = error "Words cannot contain '.'"
    | '#' `elem` str = error "Words cannot contain '#'"
    | otherwise =
        let ns = insertLetterNode (nodes w) wwf Nothing
        in WordTrie {
            nodes = ns
        }

insertMany :: WordTrie -> [WordWithFreq] -> WordTrie
insertMany = foldl' insert

insertMany1 :: WordTrie -> [String] -> WordTrie
insertMany1 = foldl' insert1

insertLetterNode :: [LetterNode] -> WordWithFreq -> Maybe LetterNode -> [LetterNode]
insertLetterNode ls ([], _) _ = ls
insertLetterNode ls ((c:cs), freq) p =
    case findLetterIndex ls c of
        Just i ->
            let
                (before, n:after) = splitAt i ls
                newNode = LetterNode {
                    char = c
                    , children = insertLetterNode (children n) (cs, freq) (Just newNode)
                    , wordFrequency = if null cs then Just freq else wordFrequency n
                    , parent = p

                }
            in
                before ++ newNode : after
        Nothing ->
            let newNode = LetterNode {
                char = c
                , children = insertLetterNode [] (cs, freq) (Just newNode)
                , wordFrequency = if null cs then Just freq else Nothing
                , parent = p
            }
            in newNode : ls

findLetterIndex :: [LetterNode] -> Char -> Maybe Int
findLetterIndex ns c = findIndex (\n -> char n == c) ns

findLetter :: [LetterNode] -> Char -> Maybe LetterNode
findLetter ns c = find (\n -> char n == c) ns

isWord :: String -> WordTrie -> Bool
isWord s wt = isJust $ getWordFrequency s wt

getWordFrequency :: String -> WordTrie -> Maybe Int
getWordFrequency [] _ = Nothing
getWordFrequency [c] w = do
    letterNode <- findLetter (nodes w) c
    wordFrequency letterNode
getWordFrequency (c:cs) w = do
    letterNode <- findLetter (nodes w) c
    getWordFrequency_ letterNode cs

getWordFrequency_ :: LetterNode -> String -> Maybe Int
getWordFrequency_ l [] = wordFrequency l
getWordFrequency_ l [c] = case findLetter (children l) c of
    Just n -> getWordFrequency_ n []
    Nothing -> Nothing
getWordFrequency_ l (c:cs) = case findLetter (children l) c of
    Just n -> getWordFrequency_ n cs
    Nothing -> Nothing

getWords :: String -> WordTrie -> [String]
getWords s wt =
    let sort = sortBy (flip compare `on` snd)
    in map fst $ sort $ getWords1 s wt

getWords1 ::  String -> WordTrie -> [WordWithFreq]
getWords1 [] _ = []
getWords1 ('.':cs) w =
    let leaves = concatMap (getLeaves cs) (nodes w)
    in produceWords leaves
getWords1 (c:cs) w = case findLetter (nodes w) c of
    Just n -> produceWords $ getLeaves cs n
    Nothing -> []

produceWords :: [LetterNode] -> [WordWithFreq]
produceWords = map readUp

readUp :: LetterNode -> WordWithFreq
readUp l = case parent l of
        Just p -> (reverse (char l : readUp_ p), wf)
        Nothing -> ([char l], wf)
        where
            wf = fromMaybe (error "Can't readUp nonword.") (wordFrequency l)

readUp_ :: LetterNode -> String
readUp_ l = case parent l of
    Just p -> char l : readUp_ p
    Nothing -> [char l]

getLeaves :: String -> LetterNode -> [LetterNode]
getLeaves [] l = [l | isJust $ wordFrequency l]
getLeaves ('.':cs) l =
    concatMap (getLeaves cs) (children l)
getLeaves (c:cs) l = case findLetter (children l) c of
    Just n -> getLeaves cs n
    Nothing -> []

