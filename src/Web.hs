{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, FlexibleContexts #-}
import Yesod
import qualified Data.Text as T
import Data.Text(Text)

import Control.Applicative ((<$>), (<*>))
import Control.Monad

import Crossword
import WebPrinter

data Cruciverbus = Cruciverbus

mkYesod "Cruciverbus" [parseRoutes|
 / HomeR GET
 /compose ComposeR GET
 /generate GenerateR POST
|]

instance Yesod Cruciverbus

instance RenderMessage Cruciverbus FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Form

data WordList = WordList {unWL :: [String]}

data GenTask = GenTask {
  gtWords :: [String],
  gtNumResults :: Int
}

blank :: (Monad m, RenderMessage master FormMessage)
      => (Text -> Either FormMessage a) -> [Text] -> m (Either (SomeMessage master) (Maybe a))
blank _ [] = return $ Right Nothing
blank _ ("":_) = return $ Right Nothing
blank f (x:_) = return $ either (Left . SomeMessage) (Right . Just) $ f x

textListField = Field
    { fieldParse = blank $ Right . map T.unpack . T.words
    , fieldView = \theId name theClass val isReq -> addHamlet
        [hamlet|\
<textarea id="#{theId}" name="#{name}" :not (null theClass):class="#{T.intercalate " " theClass}" type="text" :isReq:required=""> #{either id show_val val}
|]
}
 where show_val = T.pack . unwords

gtAForm = GenTask
  <$> areq textListField "List of words" Nothing
  <*> areq intField "Number of results to show" (Just 10)

gtForm = renderTable gtAForm

-- Request processors

getHomeR  = defaultLayout [whamlet|<a href=@{ComposeR}>Compose a crossword|]

-- Form page

getComposeR = do
  ((_, widget), enctype) <- generateFormPost gtForm
  defaultLayout [whamlet|
<p> Please input a words to build crossword of
<form method=post action=@{GenerateR} enctype=#{enctype}>
   ^{widget}
   <input type=submit value='Generate'>
|]

-- Results page

crosswords_css = [lucius|
div.crossword-container { float: left; }
.crossword { border-collapse: collapse;
  td { width: 16px; height: 16px; text-align: center; vertical-align: middle; }
  td.letter { border: 1px solid #222; }
}
div.controls { clear:both; }
|]

crosswords_snippet crosswords = [whamlet|
<p> Generated crosswords:
  $forall c <- crosswords
    <div .crossword-container> #{c}
<div.controls> <a href=@{ComposeR}> Compose another
|]

postGenerateR = do
  ((result, widget), enctype) <- runFormPost gtForm
  case result of
    FormSuccess gt -> let crosswords = buildCrosswords (gtWords gt)
                      in defaultLayout $ do toWidget crosswords_css
                                            crosswords_snippet crosswords
    _ -> defaultLayout [whamlet| Invalid input |]

main = warpDebug 3000 Cruciverbus
