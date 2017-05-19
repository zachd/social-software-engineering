{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Crawler
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Database.Bolt
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GitHubAPI


-- Type definitions
type API = "crawl" :> Get '[JSON] String


-- App setup
startApp :: IO ()
startApp = Network.Wai.Handler.Warp.run 8080 app
app :: Application
app = serve api server
api :: Proxy API
api = Proxy

-- Server
server :: Server API
server = return crawl

crawl :: String
crawl = "test"

