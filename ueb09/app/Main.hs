{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)

import Text.Blaze.Html hiding (text)
import Text.Blaze.Html.Renderer.Text
import Web.Scotty

import Types
import Website

type GBList = MVar GuestBook

main :: IO ()
main = do
  let initGB = GuestBook []
  list <- newMVar initGB
  scotty 3000 $ do 
    controller list initGB

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


controller :: GBList -> GuestBook -> ScottyM ()
controller list gB = do
  get "/" $ html $ renderHtml $ website "Happy Coffecup" "Home" $ gbContent gB
  get "/guestbook" $ listGuestBook list
  post "/guestbook" $ updateGuestBook list 

listGuestBook :: GBList -> ActionM ()
listGuestBook list = do
  result <- liftIO $ readMVar list 
  html $ renderHtml $ gbContent result

updateGuestBook :: GBList -> ActionM () 
updateGuestBook list = do
  author <- param "author"
  comment <- param "comment"
  gB <- liftIO $ takeMVar list
  liftIO $ putMVar list $ gB {entries = GuestBookEntry author comment : entries gB}
  result <- liftIO $ readMVar list
  html $ renderHtml $ gbContent result

   




