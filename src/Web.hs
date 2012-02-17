{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}
import Yesod
--import Data.Text(Text)
import Control.Applicative ((<$>), (<*>))

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

data GenTask = GenTask {
  gtWords :: Text, --[String],
  gtNumResults :: Int
}

--textListField :: (Integral i, RenderMessage master FormMessage) => Field sub master i

{-
textListField = Field
    { fieldParse = blank $ Right
    , fieldView = \theId name theClass val isReq -> addHamlet
        [WHAMLET|
 <textarea id="#{theId}" name="#{name}" :not (null theClass):class="#{T.intercalate " " theClass}" type="text" :isReq:required=""> #{either id id val}
|]
}
-}

textListField = Field {
  fieldParse = liftM unTextarea . 
                


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
