module Crossword(
    Crossword,
    Direction(ACROSS, DOWN),
    StartSquare,
    fromStrings,
    getWordAt,
    getWordAtStartSquare,
    isComplete,
    isWordComplete,
    getStartSquares,
    addWordAt) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

-- data Symmetry = NONE | HALF_TURN | QUARTER_TURN deriving Eq
data Color = LIGHT | DARK deriving Eq
data Direction = DOWN | ACROSS deriving (Eq, Show)

type Coord = (Int, Int)
type IntOp = (Int -> Int -> Int)
type StartSquare = (Coord, Direction)

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
            Crossword {
                width = w
                , height = h
                , chars = Map.fromList $ concat [[ ((x, y), charToSquare c) | (x, c) <- zip [0..] row ] | (y, row) <- zip [0..] ls ]
            }

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

getWordAtStartSquare :: Crossword -> StartSquare -> String
-- Does not check if we are at a start square!
getWordAtStartSquare cw (xy, dir) =
    getWordInBounds cw bounds dir where
        bounds = (xy, getBound cw dir (+) xy)

getWordInBounds :: Crossword -> (Coord, Coord) -> Direction -> String
getWordInBounds cw (start, end) ACROSS =
    [(squareToChar . squareAt cw) (x, snd start) | x <- [fst start .. fst end]]
getWordInBounds cw (start, end) DOWN =
    [(squareToChar . squareAt cw) (fst start, y) | y <- [snd start .. snd end]]

getBoundsAt :: Crossword -> Coord -> Direction -> (Coord, Coord)
getBoundsAt cw (x, y) dir = (getBound cw dir (-) (x, y), getBound cw dir (+) (x, y))

getBound :: Crossword -> Direction -> IntOp -> Coord -> Coord
getBound cw dir op xy@(x, y)
    | x < 0 = error "Out of bounds, x < 0"
    | x >= width cw = error "Out of bounds, x >= width"
    | y < 0 = error "Out of bounds, y < 0"
    | y >= height cw = error "Out of bounds, y >= height"
    | otherwise = if Map.member xy (chars cw)
        then getBound' cw dir op (x, y) (x, y)
        else error "Missing square in getBound"

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

coordDist :: Coord -> Coord -> Int
coordDist a b
    | fst a == fst b = snd a - snd b
    | snd a == snd b = fst a - fst b
    | otherwise = error "Can't calculate distance of coords not in a line."

isComplete :: Crossword -> Bool
isComplete c = '.' `notElem` show c

getStartSquares :: Crossword -> [StartSquare]
getStartSquares cw =
    concat [getWordStartsAt cw (x, y) |y <- [0 .. height cw], x <- [0 .. width cw]]

isDark :: Square -> Bool
isDark sq = color sq == DARK

isDarkOrEdge :: Crossword -> Coord -> Bool
isDarkOrEdge cw xy =
    -- If we don't get a square, we're out of bounds, meaning it's an edge.
    maybe True isDark (squareAt cw xy)

getWordStartsAt :: Crossword -> Coord -> [StartSquare]
getWordStartsAt c xy =
    across ++ down
    where
        across = [(xy, ACROSS) | isWordStart ACROSS c xy]
        down = [(xy, DOWN) | isWordStart DOWN c xy]


isWordStart :: Direction -> Crossword -> Coord -> Bool
isWordStart DOWN cw xy@(x,y) = not (isDarkOrEdge cw xy) && isDarkOrEdge cw (x, y - 1)
isWordStart ACROSS cw xy@(x,y) = not (isDarkOrEdge cw xy) && isDarkOrEdge cw (x - 1, y)

isWordComplete :: Crossword -> Coord -> Direction -> Bool
isWordComplete cw xy wd = '.' `elem` getWordAt cw xy wd

addWordAt :: Crossword -> Coord -> Direction -> String -> Crossword
addWordAt cw xy dir s
    | coordDist xy (getBound cw dir (+) xy) > length s =
        error "Trying to put a string in too small an area."
    | otherwise = addWordAt' cw xy dir s

addWordAt' :: Crossword -> Coord -> Direction -> String -> Crossword
addWordAt' cw _ _ [] = cw
addWordAt' cw xy@(x, y) ACROSS (c:cs) =
    addWordAt' newcw (x + 1, y) ACROSS cs
    where newcw = addLetterAt cw xy c
addWordAt' cw xy@(x, y) DOWN (c:cs) =
    addWordAt' newcw (x, y + 1) DOWN cs
    where newcw = addLetterAt cw xy c

addLetterAt :: Crossword -> Coord -> Char -> Crossword
addLetterAt cw xy c =
    -- Check to make sure the coord exists!
    case squareAt cw xy of
        Just sq -> case color sq of
            LIGHT -> cw {
                chars = Map.insert xy (charToSquare c) (chars cw)
            }
            DARK -> error "Added word would overlap dark square"
        Nothing -> error "Out of bounds coord in addLetterAt"
