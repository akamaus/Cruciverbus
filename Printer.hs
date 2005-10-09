module Printer (crosswordToString, printCrossword)

where

import Crossword (Crossword,Word(Word),Box,Point,getArea,getBoundingBox,moveAlong)
import Data.Array

getCrosswordArea :: Crossword -> Box
getCrosswordArea cr = getBoundingBox $ map getArea cr

getCrosswordArray :: Crossword -> Array Point Char
getCrosswordArray cr =
    let blank = listArray (getCrosswordArea cr) (repeat ' ')
        letters = concatMap renderWord cr :: [(Point,Char)]
    in blank // letters

renderWord :: Word -> [(Point,Char)]
renderWord (Word (l:ls) pos dir) =
    ( (pos, l) :
      renderWord (Word ls (moveAlong dir 1 pos) dir)
    )
renderWord _ = []


crosswordToString :: Crossword -> String
crosswordToString cr =
    let ((x1,y1),(x2,y2)) = getCrosswordArea cr
        array = getCrosswordArray cr :: Array Point Char
        rowIxs r = range ((x1,r),(x2,r)) :: [Point]
        row r = map (array !) $ rowIxs r :: String
        rows = map row [y1..y2] :: [String]
    in concatMap (\s -> s ++ ['\n']) rows

printCrossword :: Crossword -> IO ()
printCrossword c = putStr $ crosswordToString c
