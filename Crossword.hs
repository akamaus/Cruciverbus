module Crossword
where

import Data.List(concatMap,delete,elemIndices,(\\),sort, nub,foldl',maximumBy)
import Data.Maybe
import Data.Set(toList,fromList)

import Pole
import Word


type Crossword = [Word]

type WordList = [String]

addWord :: Crossword -> String -> [Crossword]
addWord cr str =
    let vars = concatMap (linkWord str) cr :: [Word]
        proper_vars = filter (isProper cr) vars
    in map (:cr) proper_vars

linkWord :: String -> Word -> [Word]
linkWord new_word w@(Word word place direction) = map constructWord crossings
    where crossings = findCrossings (un_wl word) new_word
          constructWord :: (Int,Int) -> Word
          constructWord (al, cr) =
              let new_place = moveAlongAndAcross direction (al,-cr) place
              in Word (mk_wl new_word) new_place (rotate direction)

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
                    aj_points = if ajacent h at then Just $ mkBox h at
                                else if ajacent t ah then Just $ mkBox t ah
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
isProperCrossing (Word l1 p1 d1) (Word l2 p2 d2) =
    case (d1,d2)
    of (Hor,Hor) -> False
       (Ver,Ver) -> False
       (Hor,_)   -> let n = (p_x p2 - p_x p1, p_y p1 - p_y p2) in checkP n
       otherwise -> let n = (p_y p2 - p_y p1, p_x p1 - p_x p2) in checkP n
    where checkP (n1,n2) = if wl_length l1 > n1 && wl_length l2 > n2 && un_wl l1 !! n1 == un_wl l2 !! n2 then True
                           else False

findCrossings :: String -> String -> [(Int,Int)]
findCrossings xs ys = findCrossings' xs 0
    where findCrossings' :: String -> Int -> [(Int,Int)]
          findCrossings' (x:xs) i =
              let cross_ixs = elemIndices x ys
                  cross_points = map (\j -> (i,j)) cross_ixs
              in cross_points ++ findCrossings' xs (i+1)
          findCrossings' _ _ = []

depth = 3

--uniq_crosswords = toList $ fromList $ crosswords

makeCrossword :: WordList -> Crossword
makeCrossword voc =
    produceCrosswords init_st
    where init_st =  [] :: Crossword
          voc_size = length voc
          produceCrosswords :: Crossword -> Crossword
          produceCrosswords cr =
              let cr_size = length cr :: Int
                  diff = voc_size - cr_size :: Int
                  vars = genCrosswords [cr] 1 :: [Crossword]
                  cur_depth = min (diff-1) depth :: Int
                  evalFork :: Crossword -> Int
                  evalFork c = maximum $ map evaluateCrossword $ genCrosswords [c] cur_depth
                  best_var = fst $ maximumBy (\a b -> compare (snd a) (snd b))
                             $ zip vars (map evalFork vars) :: Crossword
              in case diff
                 of 0 -> cr
                    1 -> head $ snd $ getBest' vars (0,[])
                    otherwise -> produceCrosswords best_var
          genCrosswords :: [Crossword] -> Int -> [Crossword]
          genCrosswords st 0 = st
          genCrosswords st n =
              let sts = st >>= forkCrossword
              in genCrosswords (nub sts) (n-1)
          forkCrossword :: Crossword -> [Crossword]
          forkCrossword [] = map seedCrossword voc
          forkCrossword cr = concatMap addCandidate candidates
              where addCandidate :: String -> [Crossword]
                    addCandidate w =
                        let crosswords = addWord cr w
                            norm_crosswords = map normalize crosswords
                        in norm_crosswords
                    candidates = voc \\ map (un_wl . letters) cr


seedCrossword :: String -> Crossword
seedCrossword str = [Word (mk_wl str) (Point 0 0) Hor]


normalize:: Crossword -> Crossword
normalize cr =
    let b = getBoundingBox $ map getBox cr
        Point px py = unbox_p1 b
        abs_cr = map (moveWord (negate px, negate py)) cr :: Crossword
        s_cr = sort abs_cr :: Crossword
        n_cr = min s_cr (reflect s_cr) :: Crossword
    in n_cr

reflect :: Crossword -> Crossword
reflect cr = map reflectWord cr

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
