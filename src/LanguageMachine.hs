module LanguageMachine (getCompletions, completeCrossword) where

import Data.List(nub)
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

getCompletions :: [WordTrie] -> String -> [String]
getCompletions _ "" = []
getCompletions wts s
    | '.' `elem` s = nub $ concatMap (getWords s) wts
    | any (isWord s) wts = [s]
    | otherwise = []

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (x:xs)
    | isJust x = x
    | otherwise = firstJust xs

extractExistingWords :: Crossword -> WordTrie
extractExistingWords cw = insertMany1 emptyWordTrie $ getAllCompleteWords cw
    where emptyWordTrie = WordTrie {nodes = []}

completeCrossword :: Crossword -> [WordTrie] -> Maybe Crossword
completeCrossword cw wts
    | isComplete cw = Just cw
    | otherwise =
        completeCrossword' cw (getStartSquares cw) (extractExistingWords cw : wts)

completeCrossword' :: Crossword -> [StartSquare] -> [WordTrie] -> Maybe Crossword
completeCrossword' cw [] _ = Just cw
completeCrossword' cw (s@(xy, dir):ss) wts = do
    let currentWord = getWordAtStartSquare cw s
    let wordsToTry = getCompletions wts currentWord
    if null wordsToTry then Nothing else do
        let potentialCrosswords = map (addWordAt cw xy dir) wordsToTry
        let completedCrosswords = map (\pcw -> completeCrossword' pcw ss wts) potentialCrosswords
        firstJust completedCrosswords
