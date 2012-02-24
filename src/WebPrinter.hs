{-# LANGUAGE QuasiQuotes, TypeSynonymInstances #-}
module WebPrinter(crosswordToHtml) where

import Yesod
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Text.Blaze(ToHtml)
import Text.Hamlet(shamlet)

import Data.Char(isSpace)
import Crossword
import Printer

crosswordToHtml cr = [shamlet|
<table .crossword>
  $forall row <- rows
    <tr>
      $forall c <- row
        $if isSpace c
          <td>#{c}
        $else
          <td .letter>#{c}
|]
 where rows = lines . crosswordToString $ cr

instance ToHtml Crossword where
  toHtml = crosswordToHtml