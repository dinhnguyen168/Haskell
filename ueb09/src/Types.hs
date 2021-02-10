module Types where

import qualified Data.Text.Lazy as T
import Data.Char 
import Data.List
import Data.Maybe (fromJust, isJust)

newtype GuestBook = GuestBook { entries :: [GuestBookEntry] } deriving Show

data Error =  CommentNotFound | UserNameNotFound    

data GuestBookEntry = GuestBookEntry 
  { author :: T.Text
  , comment :: T.Text 
  } deriving Show

data Item 
  = CafeCreme
  | LatteMachiato 
  | Milchkaffee 
  | Cappuccino 
  | Espresso 
  | Kakao 
  deriving (Enum, Read, Show)

price :: Item -> Int
price CafeCreme = 190
price LatteMachiato = 230
price Milchkaffee = 180
price Cappuccino = 220
price Espresso = 190
price Kakao = 290

showPrice :: Int -> String
showPrice p = 
  let (e,c) = divMod p 100 in show e ++ "," 
      ++ (if c < 10 then '0' : show c else show c) ++ " €"

allItems :: [Item]
allItems = enumFrom CafeCreme

data Order = OrderItem
  { item :: Item
  , quantity :: Int
  } deriving Show

priceOrder :: Order -> Int
priceOrder (OrderItem i q) = q * price i

total :: [Order] -> Int
total = foldl (\acc o -> acc + priceOrder o) 0

mkOrder :: [(T.Text, T.Text)] -> [Order]
mkOrder = filter ((>0) . quantity) . map fromJust . filter isJust . map readOrder

readOrder :: (T.Text, T.Text) -> Maybe Order
readOrder (i,q) = OrderItem <$> readItem i <*> readInt q where
  readInt :: T.Text -> Maybe Int
  readInt x = case reads (T.unpack x) of
    [(y,"")] -> Just y
    _        -> Nothing
  readItem :: T.Text -> Maybe Item
  readItem x = case reads (T.unpack x) of
    [(y,"")] -> Just y
    _        -> Nothing

data Route 
  = Home
  | Guestbook
  | Logo 
  | Stylesheet 
  | SmileImage
  | CafeImage
  | CoffeeImage

instance Show Route where
  show Home = "Home"
  show _ = "DONT USE SHOW ON THESE ITEMS!"
