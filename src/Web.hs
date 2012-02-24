{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, FlexibleContexts #-}
import Yesod
import qualified Data.Text as T
import Data.Text(Text)

import Control.Applicative ((<$>), (<*>))
import Control.Monad

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

--textListField :: (Integral i, RenderMessage master FormMessage) => Field sub master i

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

getComposeR = do
  ((_, widget), enctype) <- generateFormPost gtForm
  defaultLayout [whamlet|
<p> Please input a words to build crossword of
<form method=post action=@{GenerateR} enctype=#{enctype}>
   ^{widget}
   <input type=submit value='Generate'>
|]

postGenerateR = do
  ((result, widget), enctype) <- runFormPost gtForm
  case result of
    FormSuccess gt -> defaultLayout [whamlet|
<p> Form results:
$forall w <- gtWords gt
  <div> #{w}
<div> <a href=@{ComposeR}> Compose another
|]
    _ -> defaultLayout [whamlet| Invalid input |]

main = warpDebug 3000 Cruciverbus
