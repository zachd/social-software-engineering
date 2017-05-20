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
import Data.Aeson
import Data.Aeson.TH
import Database.Bolt
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GitHubAPI
import DatabaseAPI
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)


-- Type definitions
type API = "crawl" :> Capture "user" String :> Capture "token" String :> Get '[JSON] String


-- App setup
startApp :: IO ()
startApp = Network.Wai.Handler.Warp.run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

-- Server
server :: Server API
server = crawl

crawl :: String -> String -> Handler String
crawl user token = liftIO $ do
  addUser user
  result <- crawlUser (fromString user) token
  return user
