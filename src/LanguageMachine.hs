module LanguageMachine (getCompletions, getCompletions', completeCrossword) where

import Data.List(nub, (\\))
import Data.Maybe(isJust)

import WordTrie (WordTrie (..), isWord, getWords, insertMany1)
import Crossword (
    Crossword,
    StartSquare,
    getStartSquares,
    getWordAtStartSquare,
    addWordAt,
    isComplete,
    getAllCompleteWords)

-- A...
getCompletions :: [WordTrie] -> [String] -> String -> [String]
getCompletions _ _ "" = []
getCompletions wts used s
    | '.' `elem` s = let foundWords = nub $ concatMap (getWords s) wts
        in foundWords \\ used
    | any (isWord s) wts = [s]
    | otherwise = []

getCompletions' :: [WordTrie] -> String -> [String]
getCompletions' wts = getCompletions wts []

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (x:xs)
    | isJust x = x
    | otherwise = firstJust xs

-- Why am I taking in multiple WordTries here?
completeCrossword :: Crossword -> [WordTrie] -> Maybe Crossword
completeCrossword cw wts
    | isComplete cw = Just cw
    | otherwise = do
        let existingWords = getAllCompleteWords cw
        let existingWordsTrie = insertMany1 WordTrie {nodes = []} existingWords
        --- why am I appending existingWordsTrie to the list of wordTries?
        completeCrossword' cw (getStartSquares cw) existingWords (existingWordsTrie : wts)

completeCrossword' :: Crossword -> [StartSquare] -> [String] -> [WordTrie] -> Maybe Crossword
completeCrossword' cw [] _ _ = Just cw
completeCrossword' cw (s@(xy, dir):ss) used wts = do
    let currentWord = getWordAtStartSquare cw s
    let wordsToTry = getCompletions wts used currentWord
    if null wordsToTry then Nothing else do
        -- Can we get rid of `used` by consuming words from the Trie instead of adding them to Used
        let potentialCrosswords = zip (map (addWordAt cw xy dir) wordsToTry) wordsToTry :: [(Crossword, String)]
        let completedCrosswords = map (\pcw -> completeCrossword' (fst pcw) ss (snd pcw : used) wts) potentialCrosswords
        -- firstJust makes us hold on to the maybe elements, rewrite this so that those Nothing crosswords disappear instead?
        firstJust completedCrosswords
