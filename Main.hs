
import Crossword
import Printer

import Data.List

ws = -- ["abcd", "axy", "caz"]--, "ybze","baza"]
     ["мул","сом","енот","крот","лемур","койот","панда","тукан","мустанг","гиппопотам"]
     --["папа","мама","паша"]

cr = []
{-
cr = [ Word (ws !! 0) (0,0) Hor,
       Word (ws !! 1) (0,0) Ver,
       Word (ws !! 3) (0,2) Hor
     ]
-}

--out_crs = makeCrossword ws

--main = mapM_ (print . length . runBuilder) $ take 8 wws
main = mapM_ (print . length . buildCrosswords) $ take 8 wws
-- mapM_ (const $ putChar '.') $ runBuilder ws

--mine = mapM_ printCrossword $ snd $  runEnumerator enumerateCrosswords ["aaaaaaac","aaaab"]

wws = tail $ reverse $ tails ws