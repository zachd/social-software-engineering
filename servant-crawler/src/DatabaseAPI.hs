{-# LANGUAGE OverloadedStrings      #-}

module DatabaseAPI where

import Database.Bolt hiding(unpack)
import Data.String
import Data.Text
import Data.Map
import GitHub
import GitHub.Data.Repos
import GitHub.Data.Definitions
import qualified GitHub.Endpoints.Repos as GitHubRepos

-- Neo4J Config
config :: BoltCfg
config = def { user = "neo4j", password = "neo4j"}


-- Insert Functions
addUser :: String -> IO()
addUser user = do
    pipe <- connect config
    result <- run pipe $ queryP "CREATE (n:User {name: {name}})"
                              (fromList [("name", T (fromString user))])
    close pipe

addRepo :: Repo -> IO()
addRepo repo = do
    let owner = GitHubRepos.repoOwner repo
    let owner_name = formatName $ simpleOwnerLogin owner
    let repo_name = formatName $ GitHubRepos.repoName repo
    logMsg ["Adding Repo: ", owner_name, "/", repo_name, "\n"]
    pipe <- connect config
    result <- run pipe $ queryP "CREATE (n:Repo {owner: {owner}, name: {name}})"
                              (fromList [("owner", T(fromString owner_name)), ("name", T (fromString repo_name))])
    close pipe


-- Utility Functions
formatName :: Name a -> String
formatName = unpack . untagName

logMsg :: [String] -> IO()
logMsg = mapM_ putStr
