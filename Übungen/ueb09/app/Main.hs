{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)
import Text.Blaze.Html hiding (text)
import Text.Blaze.Html.Renderer.Text
import Web.Scotty

import Types
import Website



main :: IO ()
main = do
  myVar <- newMVar (GuestBook [])
  scotty 3000 $ do
    get "/" $ 
      html $ renderHtml $ website "Happy Coffeecup" "Home" indexContent

    get "/order.html" $ 
      html $ renderHtml $ website "Happy Coffeecup" "Bestellen" orderContent

    get "/guestbook.html" $ do
      myGb <- liftIO $ readMVar myVar
      html $ renderHtml $ website "Happy Coffeecup" "Gästebuch" (gbContent myGb)

    post "/guestbook.html" $ do
      newName <- param "author"
      newComment <- param "comment"
      liftIO $ modifyMVar_ myVar (\gb -> return (gb{entries = GuestBookEntry newName newComment : (entries gb)}))
      redirect "/guestbook.html"

    get "/receipt.html" $ do
      html $ renderHtml $ website "Happy Coffeecup" "Rechnung" (receiptContent [])

    post "/receipt.html" $ do
      txtOrders <- params
      html $ renderHtml $ website "Happy Coffeecup" "Rechnung" (receiptContent $ mkOrder txtOrders)

    -- serve static files 
    get "/static/css/bulma.css" $ do
      setHeader "Content-Type" "text/css" 
      file "static/css/bulma.css"
    get "/static/img/logo.png" $ do
      setHeader "Content-Type" "image/png" 
      file "static/img/logo.png"
    get "/static/img/smile.jpg" $ do
      setHeader "Content-Type" "image/jpg" 
      file "static/img/smile.jpg"
    get "/static/img/coffee.jpg" $ do
      setHeader "Content-Type" "image/jpg" 
      file "static/img/coffee.jpg"
    get "/static/img/cafe.jpg" $ do
      setHeader "Content-Type" "image/jpg" 
      file "static/img/cafe.jpg"
