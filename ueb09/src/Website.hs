{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
module Website where

import Text.Blaze
import Text.Blaze.Html
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (title, form, item)
import Text.Hamlet
import Prelude hiding (div, id, head)
import Control.Concurrent.MVar
import Control.Monad

import Types

renderUrl :: Render Route
renderUrl Home _ = "/"
renderUrl Guestbook _ = "/guestbook"
renderUrl Logo _ = "/static/img/logo.png"
renderUrl Stylesheet _ = "/static/css/bulma.css"
renderUrl SmileImage _ = "/static/img/smile.jpg"
renderUrl CafeImage _ = "/static/img/cafe.jpg"
renderUrl CoffeeImage _ = "/static/img/coffee.jpg"

metaWebsite :: String -> String -> Html
metaWebsite title subtitle = $(hamletFile "templates/meta.template") renderUrl

website :: String -> String -> Html -> Html
website title subtitle content = docTypeHtml $ do
    metaWebsite title subtitle
    body $ do
        $(hamletFile "templates/navbar.template") renderUrl --navigation Bar
        main ! class_ "main" $
            div ! class_ "container" $
                div ! class_ "box" $ do
                    h1 ! class_ "title" $ "Willkommen im Café Happy Coffeecup!"
                    p ! class_ "mb-5 mt-5" $ "Kommen Sie uns im wunderschönen Bremen besuchen und probieren Sie unseren erstklassigen Kaffee."
                    p ! class_ "mb-5 mt-5" $ ""
                    div ! class_ "tile is-ancestor" $ do
                        div ! class_ "tile is-parent" $
                            div ! class_ "tile is-child box" $
                                figure ! class_ "image is-4by3" $
                                    [hamlet|<img class="has-ratio" src=@{SmileImage}>|] renderUrl
                        div ! class_ "tile is-parent is-6" $
                            div ! class_ "tile is-child box" $
                                figure ! class_ "image is-4by3" $
                                    [hamlet|<img class="has-ratio" src=@{CoffeeImage}>|] renderUrl
                    div ! class_ "tile is-ancestor" $
                        div ! class_ "tile is-parent" $
                            div ! class_ "tile is-child box" $
                                figure ! class_ "image is-16by9" $
                                    [hamlet|<img class="has-ratio" src=@{CafeImage}>|] renderUrl
-- Aufgabe 2:

gbContent :: GuestBook -> Html
gbContent gB = docTypeHtml $ do 
    metaWebsite "Happy Coffecup" "Home"
    body $ do 
        $(hamletFile "templates/navbar.template") renderUrl --navigation Bar
        main ! class_ "main" $
            div ! class_ "container" $
                div ! class_ "box" $ do
                    h1 ! class_ "title" $ "Gästebuch"
                    p ! class_ "mb-5 mt-5" $ ""
                    commentListe gB 
                    h1 ! class_ "title is-4" $ "Dein Kommentar"
                    form ! action "guestbook.html" ! method "POST" ! id "newEntry" ! name "newEntry" $ do
                        div ! class_ "field" $ do
                            div ! class_ "label" $ "Name"
                            div ! class_ "control" $ 
                                input ! class_ "input" ! type_ "text" ! id "author" ! name "author"
                        div ! class_ "field" $ do
                            div ! class_ "label" $ "Kommentar"
                            div ! class_ "control" $ 
                                textarea ! class_ "textarea" ! id "comment" ! name "comment" $ ""
                        div ! class_ "field is-grouped" $ do
                            div ! class_ "control" $
                                button ! class_ "button is-link" ! type_ "submit" $ "Absenden"
                            div ! class_ "control" $
                                button ! class_ "button is-link is-light" ! type_ "reset" $ "Abbrechen!"
                                

commentListe :: GuestBook -> Html 
commentListe gB = 
    forM_ (entries gB) $ \x -> do
        p $ b $ toHtml $ author x
        p $ toHtml $ comment x
        p ! class_ "mb-5 mt-5" $ ""

