{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

module Crawler
    ( startApp
    , app
    ) where

import Data.Text
import Data.String
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GitHubAPI
import DatabaseAPI
import Control.Monad.IO.Class (liftIO)


-- Type definitions
type API = "user" :> Capture "user" String :> Capture "token" String :> Get '[JSON] String
        :<|> "org" :> Capture "user" String :> Capture "token" String :> Get '[JSON] String


-- App setup
startApp :: IO ()
startApp = Network.Wai.Handler.Warp.run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

-- Server
server :: Server API
server = user
    :<|> org

user :: String -> String -> Handler String
user name token = liftIO $ do
  addUser name
  result <- crawlUser (fromString name) token
  return "Crawl Complete"

org :: String -> String -> Handler String
org name token = liftIO $ do
  addOrg name
  result <- crawlOrg (fromString name) token
  return "Crawl Complete"
