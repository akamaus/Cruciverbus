module Printer (crosswordToString, printCrossword) where

import Pole
import Word
import Crossword(Crossword)

import Data.Array

getCrosswordArea :: Crossword -> Box
getCrosswordArea = getBoundingBox . map getArea

getCrosswordArray :: Crossword -> Array Point Char
getCrosswordArray cr =
    let blank = listArray (unbox $ getCrosswordArea cr) (repeat ' ')
        letters = concatMap renderWord cr :: [(Point,Char)]
    in blank // letters

renderWord :: Word -> [(Point,Char)]
renderWord (Word wl pos dir) = zip (iterate (moveAlong dir 1) pos) (un_wl wl)

crosswordToString :: Crossword -> String
crosswordToString cr =
    let b = getCrosswordArea cr
        (p1,p2) = (unbox_p1 b, unbox_p2 b)
        array = getCrosswordArray cr :: Array Point Char
        rowIxs r = range (p1{p_y = r},p2{p_y = r}) :: [Point]
        row r = map (array !) $ rowIxs r :: String
        rows = map row [p_y p1 .. p_y p2] :: [String]
    in concatMap (\s -> s ++ ['\n']) rows

printCrossword :: Crossword -> IO ()
printCrossword c = putStr $ crosswordToString c
