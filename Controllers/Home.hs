{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Controllers.Home
    ( home
    , addMessage
    , messages
    ) where

import           Views.Home (homeView)
import           Web.Scotty (ActionM, ScottyM, get, html, json, jsonData, param, post)
import           Data.Aeson ((.:), FromJSON, ToJSON, parseJSON, withObject)
import           GHC.Generics
import           Data.Text

home :: ScottyM ()
home = get "/" homeView

-- Messages handling

data Message = Message
  { name        :: String
  , email       :: String
  , messageText :: String } deriving Generic

instance FromJSON Message where
  parseJSON = withObject "Message" $ \m -> Message
    <$> m .: "name"
    <*> m .: "email"
    <*> m .: "messageText"

instance ToJSON Message

writeMessage :: ActionM ()
writeMessage = do
  t <- jsonData
  json (t :: Message)

addMessage :: ScottyM ()
addMessage = post "/add-message" writeMessage

messages :: ScottyM()
messages = get "/messages" $ json [
    Message "Bertrand" "sample@ndd.com" "Ceci est mon message",
    Message "Bertrand" "sample@ndd.com" "Ceci est mon message"
  ]
