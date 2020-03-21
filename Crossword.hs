module Crossword(Crossword, Direction(ACROSS, DOWN), fromStrings, getWordAt) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

-- data Symmetry = NONE | HALF_TURN | QUARTER_TURN deriving Eq
data Color = LIGHT | DARK deriving Eq
data Direction = DOWN | ACROSS deriving (Eq, Show)

type Coord = (Int, Int)
type IntOp = (Int -> Int -> Int)
data Square = Square {char :: Maybe Char, color :: Color}
data Crossword = Crossword {chars :: Map.Map Coord Square, width :: Int, height :: Int}

instance Show Crossword where
    show c = unlines $ map (showRow c [0..width c - 1]) [0..height c - 1]

showRow :: Crossword -> [Int] -> Int -> String
showRow c xs y = [squareToChar (squareAt c (x, y)) | x <- xs]

fromStrings :: [String] -> Crossword
fromStrings ls
    | null ls = Crossword {width = 0, height = 0, chars = Map.empty}
    | otherwise =
        let
            (w,h) = getStringsBounds ls
        in
            let cw = Crossword {
                width = w
                , height = h
                , chars = Map.fromList $ concat [[ ((x, y), charToSquare c) | (x, c) <- zip [0..] row ] | (y, row) <- zip [0..] ls ]
            } in trace (show cw) cw

getStringsBounds :: [String] -> Coord
getStringsBounds [] = (0,0)
getStringsBounds ss =
    if all ((== (length $ head ss)) . length) (tail ss)
    then (length (head ss), length ss)
    else error "Input was not a rectangle"

squareAt :: Crossword -> Coord -> Maybe Square
squareAt c xy = chars c Map.!? xy

charToSquare :: Char -> Square
charToSquare c
    | c == '#' = Square {char = Nothing, color = DARK}
    | c == ' ' = Square {char = Nothing, color = LIGHT}
    | c == '.' = Square {char = Nothing, color = LIGHT}
    | otherwise = Square {char = Just c, color = LIGHT}

squareToChar :: Maybe Square -> Char
squareToChar ms =
    case ms of
        Just s -> case color s of
            LIGHT -> fromMaybe '.' (char s)
            DARK -> '#'
        Nothing -> error "Out of bounds square access"

getWordAt :: Crossword -> Coord -> Direction -> String
getWordAt cw xy dir =
    let
        bounds = getBoundsAt cw xy dir
    in
        getWordInBounds cw bounds dir

getWordInBounds :: Crossword -> (Coord, Coord) -> Direction -> String
getWordInBounds cw (start, end) ACROSS =
    trace (show (start, end)) $
    [(squareToChar . squareAt cw) (x, snd start) | x <- [fst start .. fst end]]
getWordInBounds cw (start, end) DOWN =
    [(squareToChar . squareAt cw) (fst start, y) | y <- [snd start .. snd end]]

getBoundsAt :: Crossword -> Coord -> Direction -> (Coord, Coord)
getBoundsAt cw (x, y) dir = (getBound cw dir (-) (x, y), getBound cw dir (+) (x, y))

getBound :: Crossword -> Direction -> IntOp -> Coord -> Coord
getBound cw dir op (x, y)
    | x < 0 = error "Out of bounds, x < 0"
    | x > width cw = error "Out of bounds, x > width"
    | y < 0 = error "Out of bounds, y < 0"
    | y > height cw = error "Out of bounds, y > height"
    | otherwise = case squareAt cw (x, y) of
        Just _ -> getBound' cw dir op (x, y) (x, y)
        Nothing -> error "Missing square in getBound"

getBound' :: Crossword -> Direction -> IntOp -> Coord -> Coord -> Coord
getBound' cw dir op xy prevc =
    case squareAt cw xy of
        Just sq -> case color sq of
            LIGHT -> getBound' cw dir op (nextCoord op dir xy) xy
            DARK -> prevc
        Nothing -> prevc

nextCoord :: IntOp -> Direction -> Coord -> Coord
nextCoord op ACROSS (x, y) = (x `op` 1, y)
nextCoord op DOWN (x, y) = (x, y `op` 1)
