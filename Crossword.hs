module Crossword
where

import Data.List(concatMap,delete,elemIndices,(\\),sort, nub,foldl')
import Data.Maybe
import Data.Set(toList,fromList)


data Word = Word { letters :: String,
                   pos :: Point,
                   dir :: Direction} deriving (Show,Eq,Ord)

tail_pos :: Word -> Point
tail_pos (Word l p d) = moveAlong d (length l - 1) p

data Direction = Hor | Ver deriving (Show,Eq,Ord)
rotate :: Direction -> Direction
rotate Hor = Ver
rotate Ver = Hor

type Point = (Int,Int)

type Box = (Point,Point)

type Crossword = [Word]

type WordList = [String]



addWord :: Crossword -> String -> [Crossword]
addWord cr str =
    let vars = concatMap (linkWords str) cr :: [Word]
        proper_vars = filter (isProper cr) vars
    in map (:cr) proper_vars

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

isProper :: Crossword -> Word -> Bool
isProper cr w =
    let new_word_box = getBox w :: Box
        close = filter (boxesCrossing new_word_box . getArea) cr :: [Word]
        crossing = filter (boxesCrossing new_word_box . getBox) close :: [Word]
        adjacent = close \\ crossing :: [Word]
        dir_w = dir w
        testAjacent aw
            | dir_w /= dir aw = False
            | otherwise =
                let h = pos w
                    t = tail_pos w
                    ah = pos aw
                    at = tail_pos aw
                    aj_points = if ajacent h at then Just (h,at)
                                else if ajacent t ah then Just (t,ah)
                                     else Nothing
                in case aj_points of
                                  Nothing -> False
                                  (Just ps) -> or $ map (isSubbox ps . getBox) crossing
        ajacent :: Point -> Point -> Bool
        ajacent p1 p2 = p2 == moveAlong (rotate dir_w) 1 p1 ||
                        p2 == moveAlong (rotate dir_w) (-1) p1
    in (and $ map (isProperCrossing w) crossing) &&
       (and $ map testAjacent adjacent)



isProperCrossing :: Word -> Word -> Bool
isProperCrossing (Word l1 (px1,py1) d1) (Word l2 (px2,py2) d2) =
    case (d1,d2)
    of (Hor,Hor) -> False
       (Ver,Ver) -> False
       (Hor,_)   -> let p = (px2-px1,py1-py2) in checkP p
       otherwise -> let p = (py2-py1,px1-px2) in checkP p
    where checkP (n1,n2) = if length l1 > n1 && length l2 > n2 && l1 !! n1 == l2 !! n2 then True
                           else False


findCrossings :: String -> String -> [(Int,Int)]
findCrossings xs ys = findCrossings' xs 0
    where findCrossings' :: String -> Int -> [(Int,Int)]
          findCrossings' (x:xs) i =
              let cross_ixs = elemIndices x ys
                  cross_points = map (\j -> (i,j)) cross_ixs
              in cross_points ++ findCrossings' xs (i+1)
          findCrossings' _ _ = []

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

isSubbox :: Box -> Box -> Bool
isSubbox ((xs1,ys1),(xs2,ys2)) ((xL1,yL1),(xL2,yL2)) =
    xL1 <= xs1 && xs2 <= xL2 &&
    yL1 <= ys1 && ys2 <= yL2

boxesCrossing :: Box -> Box -> Bool
boxesCrossing ((x1,y1),(x2,y2))
                     ((u1,v1),(u2,v2))
    | x2 < u1 || u2 < x1 ||
      y2 < v1 || v2 < y1 = False
    | otherwise = True

depth = 4

--uniq_crosswords = toList $ fromList $ crosswords

makeCrossword :: WordList -> [Crossword]
makeCrossword words =
    genCrosswords [init_st] depth
    where init_st =  [] :: Crossword
          genCrosswords :: [Crossword] -> Int -> [Crossword]
          genCrosswords st 0 = st
          genCrosswords st n =
              let sts = st >>= forkCrossword
              in genCrosswords (nub sts) (n-1)
          forkCrossword :: Crossword -> [Crossword]
          forkCrossword [] = map seedCrossword words
          forkCrossword cr = concatMap addCandidate candidates
              where addCandidate :: String -> [Crossword]
                    addCandidate w =
                        let crosswords = addWord cr w
                            norm_crosswords = map normalize crosswords
                        in norm_crosswords
                    candidates = words \\ map letters cr


seedCrossword :: String -> Crossword
seedCrossword str = [Word str (0,0) Hor]





normalize:: Crossword -> Crossword
normalize cr =
    let ((bx,by),_) = getBoundingBox $ map getBox cr
        moveWord (Word w (px,py) d)  (x,y) = Word w (px+x, py+y) d
        abs_cr = map (moveWord `flip` (-bx, -by)) cr :: Crossword
        s_cr = sort abs_cr :: Crossword
        n_cr = max s_cr  (reflect s_cr) :: Crossword
    in n_cr

reflect :: Crossword -> Crossword
reflect cr = map reflectWord cr
    where reflectWord (Word w (x,y) dir) = Word w (y,x) (rotate dir)


evaluateCrossword :: Crossword -> Int
evaluateCrossword cr =
    let boxes = map getBox cr :: [Box]
        crossings = map (fromEnum . uncurry boxesCrossing) $ makePairs boxes :: [Int]
    in foldl' (+) 0 crossings


makePairs :: [a] -> [(a,a)]
makePairs (x:xs) = map (\t -> (x,t)) xs ++ makePairs xs
makePairs [] = []

{-
getBest :: WordList -> [Crossword]
getBest lst =
    let crs = makeCrossword lst
    in snd $ getBest' crs (0,[])
-}

getBest' :: [Crossword] -> (Int, [Crossword]) -> (Int, [Crossword])
getBest' (cr:crs) best@(best_eval, best_crs) =
    let cur = evaluateCrossword cr
    in  case compare cur best_eval
        of LT -> getBest' crs best
           EQ -> getBest' crs (best_eval, cr:best_crs)
           GT -> getBest' crs (cur, [cr])

getBest' [] best = best
