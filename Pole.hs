module Pole(Point(..), Box, mkBox, mkAtomBox, unbox_p1, unbox_p2, unbox,
            Direction(..), rotate,
            moveAlong, moveAlongAndAcross, ajacent,
            isSubbox, boxesCrossing, getBoundingBox,
            intersect, corners) where

import Data.Array

data Point = Point {p_x::Int,p_y::Int} deriving (Eq, Ord,Show, Ix)

data Box = Box Point Point deriving (Show,Eq)
unbox_p1 (Box p _) = p
unbox_p2 (Box _ p) = p

unbox (Box p1 p2) = (p1,p2)

mkBox p1 p2 = if p1 < p2 then Box p1 p2 else Box p2 p1
mkAtomBox p = mkBox p p

data Direction = Hor | Ver deriving (Show,Eq,Ord)
rotate :: Direction -> Direction
rotate Hor = Ver
rotate Ver = Hor

moveAlong :: Direction -> Int -> Point -> Point
moveAlong Hor dist p = p{p_x = p_x p + dist}
moveAlong Ver dist p = p{p_y = p_y p + dist}
moveAlongAndAcross :: Direction -> (Int,Int) -> Point -> Point
moveAlongAndAcross dir (al,ac) p = (moveAlong (rotate dir) ac . moveAlong dir al) p

ajacent :: Direction -> Point -> Point -> Bool
ajacent d p1 p2 = p2 == moveAlong d 1 p1 ||
                  p2 == moveAlong d (-1) p1

topLefter (Point x1 y1) (Point x2 y2) = x1 <= x2 && y1 <= y2

getBoundingBox :: [Box] -> Box
getBoundingBox = foldl1 (\(Box pm pM) (Box p1 p2) ->
                             Box (Point (min (p_x pm) (p_x p1)) (min (p_y pm) (p_y p1)))
                                 (Point (max (p_x pM) (p_x p2)) (max (p_y pM) (p_y p2)))
                        )

intersect (Box pM pm) (Box p1 p2) =
    Box (Point (max (p_x pM) (p_x p1)) (max (p_y pM) (p_y p1)))
            (Point (min (p_x pm) (p_x p2)) (min (p_y pm) (p_y p2)))

corners (Box p1@(Point p1x p1y) p2@(Point p2x p2y)) =
    [p1, Point p2x p1y, p2, Point p1x p2y]

isSubbox :: Box -> Box -> Bool
isSubbox (Box p1s p2s) (Box p1L p2L) = topLefter p1L p1s && topLefter p2s p2L

boxesCrossing :: Box -> Box -> Bool
boxesCrossing b1 b2 =
    let (bl,br) = if unbox_p1 b1 < unbox_p1 b2 then (b1,b2) else (b2,b1)
    in between Hor (unbox_p1 bl) (unbox_p1 br) (unbox_p2 bl) &&
       ( between Ver (unbox_p1 bl) (unbox_p2 br) (unbox_p2 bl) ||
         between Ver (unbox_p1 bl) (unbox_p1 br) (unbox_p2 bl) )


-- p2 is between p1 and p3 on given direction
between :: Direction -> Point -> Point -> Point -> Bool
between d p1 p2 p3 = case d of
                         Hor -> p_x p1 <= p_x p2 && p_x p2 <= p_x p3
                         Ver -> p_y p1 <= p_y p2 && p_y p2 <= p_y p3

