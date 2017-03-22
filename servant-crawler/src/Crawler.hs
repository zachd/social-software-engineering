{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Crawler
    ( startApp
    , app
    ) where

import Data.Text
import Data.Vector
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import GitHub
import GitHub.Data.Repos
import qualified GitHub.Endpoints.Repos as GitHubRepos
import qualified GitHub.Endpoints.Users.Followers as GitHubFollowers

data Userx = Userx
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''Userx)

type API = "users" :> Get '[JSON] [Userx]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [Userx]
users = [ Userx 1 "Isaac" "Newton"
        , Userx 2 "Albert" "Einstein"
        ]

formatRepo :: Repo -> Text
formatRepo = GitHubRepos.untagName . GitHub.Data.Repos.repoName

formatUser :: SimpleUser -> Text
formatUser = GitHub.untagName . GitHub.simpleUserLogin

getRepos :: Text -> IO (Vector Text)
getRepos name = do
    let owner = GitHub.mkOwnerName name
    request <- GitHubRepos.userRepos owner GitHub.Data.Repos.RepoPublicityAll
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ Data.Vector.map formatRepo result

getFollowers :: Text -> IO (Vector Text)
getFollowers name = do
    let user = GitHub.mkUserName name
    request <- GitHubFollowers.usersFollowing user
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ Data.Vector.map formatUser result
