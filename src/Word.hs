module Word(WordLetters, mk_wl, un_wl, wl_length,
            Word(..),
            getArea, getBox, tail_pos,
            moveWord, reflectWord) where
import Pole

newtype WordLetters = WL String deriving (Show,Eq,Ord)

mk_wl = WL
un_wl (WL wl) = wl
wl_length = length . un_wl

data Word = Word { letters :: WordLetters,
                   pos :: Point,
                   dir :: Direction} deriving (Show,Eq, Ord)

getArea :: Word -> Box
getArea (Word w p d) =
    let len = wl_length w
        top_left = moveAlongAndAcross d (-1,-1) p
        bottom_right = moveAlongAndAcross d (len,1) p
    in mkBox top_left bottom_right

getBox :: Word -> Box
getBox (Word w p d) =
    let len = wl_length w
        top_left = p
        bottom_right = moveAlong d (len-1) p
    in mkBox top_left bottom_right

tail_pos :: Word -> Point
tail_pos = unbox_p2 . getBox

moveWord (x,y) (Word w (Point px py) d) = Word w (Point (px+x) (py+y)) d

reflectWord (Word w (Point x y) dir) = Word w (Point y x) (rotate dir)