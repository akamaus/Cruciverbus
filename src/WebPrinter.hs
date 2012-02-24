{-# LANGUAGE QuasiQuotes, TypeSynonymInstances #-}
module WebPrinter(crosswordToHtml) where

import Yesod
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Text.Blaze(ToHtml)
import Text.Hamlet(shamlet)

import Crossword
import Printer


crosswordToHtml cr = [shamlet|
<table>
  $forall row <- rows
    <tr>
      $forall c <- row
        <td>#{c}
|]
 where rows = lines . crosswordToString $ cr

instance ToHtml Crossword where
  toHtml = crosswordToHtml