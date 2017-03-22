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


data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

-- Type definitions
type API = "users" :> Get '[JSON] [User]
           :<|> "albert" :> Get '[JSON] User
           :<|> "isaac" :> Get '[JSON] User

-- App setup
startApp :: IO ()
startApp = Network.Wai.Handler.Warp.run 8080 app
app :: Application
app = serve api server
api :: Proxy API
api = Proxy

-- Server
server :: Server API
server = return users
     :<|> return albert
     :<|> return isaac

-- Handlers
users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

isaac :: User
isaac = User 1 "Isaac" "Newton"

albert :: User
albert = User 2 "Albert" "Einstein"
