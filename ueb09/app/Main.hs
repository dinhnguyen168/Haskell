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
  gB <- newMVar $ GuestBook []
  scotty 5000 $ do 
    controller gB
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


controller :: MVar a -> ScottyM ()
controller gB = do
    get "/" $ 
      html $ renderHtml $ website "Happy Coffecup" "Home" $ metaWebsite "Happy Coffecup" "Home"
    get "/guestbook" $
      html $ renderHtml $ gbContent (GuestBook [])
