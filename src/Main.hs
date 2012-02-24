
import Crossword
import Printer

import Word
import Pole

import Text.Printf

import Data.List

ws = -- ["abcd", "axy", "caz"]--, "ybze","baza"]
     --["мул","сом","енот","крот","лемур","койот","панда","тукан","мустанг","гиппопотам"]
     --["папа","мама","паша"]
     words "кино окно дом"

{-
cr = [ Word (ws !! 0) (0,0) Hor,
       Word (ws !! 1) (0,0) Ver,
       Word (ws !! 3) (0,2) Hor
     ]
-}

cr = [Word (WL "окно") (Point 0 0) Hor,
      Word (WL "кино") (Point 3 (-3)) Ver]

cr2 = [Word (WL "дом") (Point 2 0) Hor,
      Word (WL "кино") (Point 3 (-3)) Ver]


--out_crs = makeCrossword ws

--main = mapM_ (print . length . runBuilder) $ take 8 wws
main = mapM_ print_nth $ take 8 wws
  where print_nth wl = do printf "All crosswords of %d words\n" (length wl)
                          mapM printCrossword $ buildCrosswords wl

-- mapM_ (const $ putChar '.') $ runBuilder ws

--mine = mapM_ printCrossword $ snd $  runEnumerator enumerateCrosswords ["aaaaaaac","aaaab"]

wws = tail $ reverse $ tails ws

--isProper cr2 (Word (WL "окно") (Point 2 (-3)) Hor)