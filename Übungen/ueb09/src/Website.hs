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
renderUrl Logo _ = "/static/img/logo.png"
renderUrl Stylesheet _ = "/static/css/bulma.css"
renderUrl SmileImage _ = "/static/img/smile.jpg"
renderUrl CafeImage _ = "/static/img/cafe.jpg"
renderUrl CoffeeImage _ = "/static/img/coffee.jpg"
renderUrl Guestbook _ = "/guestbook.html"
renderUrl Order _ = "/order.html"
renderUrl Receipt _ = "/receipt.html"


metaWebsite :: String -> String -> Html
metaWebsite title subtitle = $(hamletFile "templates/meta.template") renderUrl

website :: String -> String -> Html -> Html
website title subtitle content = docTypeHtml $ do
  metaWebsite title subtitle
  body $ do
    $(hamletFile "templates/nav.template") renderUrl
    main ! class_ "main" $ do
      div ! class_ "container" $ do
        div ! class_ "box" $ do
          content


indexContent :: Html
indexContent = $(hamletFile "templates/index.template") renderUrl

showGBEntry :: GuestBookEntry -> Html
showGBEntry entry = html $ do
  p $ do
    b $ (toHtml $ author entry)
  p $ (toHtml $ comment entry)
  p ! class_ "mb-5 mt-5" $ ""

gbContent :: GuestBook -> Html
gbContent gb = html $ do
  h1 ! class_ "title" $ "Gästebuch"
  p ! class_ "mb-5 mt-5" $ ""
  mconcat $ map showGBEntry (entries gb)
  h1 ! class_ "title is-4" $ "Dein Kommentar"
  form ! action "/guestbook.html" ! method "post" ! id "newEntry" ! name "newEntry" $ do
    div ! class_ "field" $ do
      div ! class_ "label" $ "Name"
    div ! class_ "control" $ do
      input ! class_ "input" ! type_ "text" ! id "author" ! name "author"
    div ! class_ "field" $ do
      div ! class_ "label" $ "Kommentar"
      div ! class_ "control" $ do
        textarea ! class_ "textarea" ! id "comment" ! name "comment" $ ""
    div ! class_ "field is-grouped" $ do
      div ! class_ "control" $ do
        button ! class_ "button is-link" ! type_ "submit" $ "Absenden"
      div ! class_ "control" $ do
        button ! class_ "button is-link is-light" ! type_ "reset" $ "Abbrechen!"

showOrderItem :: Item -> Html
showOrderItem i = html $ do
  tr $ do
    td $ do
      div ! class_ "field" $ do
        div ! class_ "control" $ do
          div ! class_ "select" $ do
            select ! class_ "select" ! name (stringValue $ show i) ! id (stringValue $ show i) $ do
              forM_ [0..9] (option . string . show)

    td $ do
      p ! class_ "content" $ (toHtml $ show i)
    td $ do
      p ! class_ "content" $ (toHtml $ showPrice $ price i)

orderContent :: Html
orderContent = html $ do
  h1 ! class_ "title" $ "Bestellen"
  p ! class_ "mb-5 mt-5" $ ""
  form ! action "/receipt.html" ! method "post" ! id "newEntry" ! name "newEntry" $ do
    table ! class_ "table" $ do
      tbody $ do
        tr $ do
          th $ "Anzahl"
          th $ "Gericht"
          th $ "Preis"
        
        mconcat $ map showOrderItem allItems
    div ! class_ "field is-grouped" $ do
      div ! class_ "control" $ do
        button ! class_ "button is-link" ! type_ "submit" $ "Absenden"
      div ! class_ "control" $ do
        button ! class_ "button is-link is-light" ! type_ "reset" $ "Abbrechen!"


showReceiptItem :: Order -> Html
showReceiptItem o = html $ do
  tr $ do
    td $ (toHtml $ show $ quantity o) 
    td $ (toHtml $ show $ item o)
    td $ (toHtml $ showPrice $ priceOrder o)

receiptContent :: [Order] -> Html
receiptContent orders = html $ do
  h1 ! class_ "title" $ "Rechnung"
  p ! class_ "mt-5 mb-5" $ "Vielen Dank für Ihre Bestellung!"
  table ! class_ "table mt-5 mb-5" $ do
    tbody $ do
      tr $ do
        th $ "Anzahl"
        th $ "Gericht"
        th $ "Preis"
      
      mconcat $ map showReceiptItem orders
  p ! class_ "mt-5 mb-5" $ do
    b $ (toHtml $ "Gesamtpreis: " ++ (showPrice $ total orders))