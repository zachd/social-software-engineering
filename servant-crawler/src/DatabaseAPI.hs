{-# LANGUAGE OverloadedStrings #-}

module DatabaseAPI where

import Database.Bolt hiding(pack, unpack)
import Data.String
import Data.Text
import Data.Map
import GitHub hiding(query)
import GitHub.Data.Repos
import GitHub.Data.Definitions
import qualified GitHub.Endpoints.Repos as GitHubRepos hiding(query)

-- Neo4J Config
config :: BoltCfg
config = def { user = "neo4j", password = "neo4j"}


-- Insert Functions
addOrg :: String -> IO()
addOrg org = do
    pipe <- connect config
    result <- run pipe $ query $ pack $ "MERGE (a:" ++ (getNode "Org" org) ++ ")"
    close pipe

addUser :: String -> IO()
addUser user = do
    pipe <- connect config
    result <- run pipe $ query $ pack $ "MERGE (a:" ++ (getNode "User" user) ++ ")"
    close pipe

addRepo :: String -> (String, String) -> IO()
addRepo node_type (owner_name, repo_name) = do
    logMsg ["Adding Repo: ", owner_name, "/", repo_name, "\n"]
    pipe <- connect config
    result <- run pipe $ query $ pack $ "MERGE (a:" ++ (getRepoNode owner_name repo_name) ++ ")"
    result <- run pipe $ query $ pack $ addLink (getNode node_type owner_name) (getRepoNode owner_name repo_name) "OWNER"
    close pipe

addContributor :: (String, String) -> String -> IO()
addContributor (owner_name, repo_name) contrib_name = do
    logMsg ["Adding Contributor: ", contrib_name, "\n"]
    pipe <- connect config
    result <- run pipe $ query $ pack $ "MERGE (a:" ++ (getNode "User" contrib_name) ++ ")"
    result <- run pipe $ query $ pack $ addLink (getNode "User" contrib_name) (getRepoNode owner_name repo_name) "CONTRIBUTOR"
    close pipe

getRepoNode :: String -> String -> String
getRepoNode owner name = "Repo {owner: '" ++ owner ++ "', name: '" ++ name ++ "'}"

getNode :: String -> String -> String
getNode node_type name = node_type ++ " {name: '" ++ name ++ "'}"

addLink :: String -> String -> String -> String
addLink a b link = "MATCH (a:" ++ a ++ ") MATCH (b:" ++ b ++ ") MERGE (a)-[l:" ++ link ++ "]->(b)"

-- Utility Functions
logMsg :: [String] -> IO()
logMsg = mapM_ putStr
