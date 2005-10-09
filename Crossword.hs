module Crossword
where

data Word = Word { letters :: String,
                   pos :: Point,
                   dir :: Direction}

data Direction = Hor | Ver
rotate :: Direction -> Direction
rotate Hor = Ver
rotate Ver = Hor

type Point = (Int,Int)

type Box = (Point,Point)

type Crossword = [Word]



addWord :: Crossword -> String -> [Crossword]
addWord cr str =
    let vars = concatMap (linkWords str) cr :: [Word]
        proper_vars = filter (isProper cr) vars
    in map (:cr) vars

linkWords :: String -> Word -> [Word]
linkWords new_word w@(Word word place direction) = map constructWord crossings
    where crossings = findCrossings word new_word
          constructWord :: (Int,Int) -> Word
          constructWord (al, cr) =
              let new_place = moveAlongAndAcross direction (al,-cr) place
              in Word new_word new_place (rotate direction)

moveAlong :: Direction -> Int -> Point -> Point
moveAlong Hor dist (x,y) = (x+dist, y)
moveAlong Ver dist (x,y) = (x, y+dist)
moveAlongAndAcross :: Direction -> (Int,Int) -> Point -> Point
moveAlongAndAcross dir (al,ac) p = (moveAlong (rotate dir) ac . moveAlong dir al) p

isProper _ _ = True

findCrossings :: String -> String -> [(Int,Int)]
findCrossings _ _ = [(0,0)]

getArea :: Word -> Box
getArea (Word w p d) =
    let len = length w
        top_left = moveAlongAndAcross d (-1,-1) p
        bottom_right = moveAlongAndAcross d (len,1) p
    in (top_left, bottom_right)

getBox :: Word -> Box
getBox (Word w p d) =
    let len = length w
        top_left = p
        bottom_right = moveAlong d (len-1) p
    in (top_left, bottom_right)

getBoundingBox :: [Box] -> Box
getBoundingBox = foldl1 (\((xm,ym),(xM,yM)) ((x1,y1),(x2,y2)) ->
                             ((min xm x1, min ym y1), (max xM x2, max yM y2)))

makeCrossword :: [String] -> Crossword
makeCrossword strs = []

seedCrossword :: String -> Crossword
seedCrossword str = [Word str (0,0) Hor]

