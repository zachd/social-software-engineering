{-# LANGUAGE OverloadedStrings      #-}

module DatabaseAPI where

import Database.Bolt hiding(unpack)
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
addUser :: String -> IO()
addUser user = do
    pipe <- connect config
    result <- run pipe $ queryP "CREATE (n:User {name: {name}})"
                              (fromList [("name", T (fromString user))])
    close pipe

addRepo :: Text -> Repo -> IO()
addRepo user repo = do
    let owner = GitHubRepos.repoOwner repo
    let owner_name = formatName $ simpleOwnerLogin owner
    let repo_name = formatName $ GitHubRepos.repoName repo
    logMsg ["Adding Repo: ", owner_name, "/", repo_name, "\n"]
    pipe <- connect config
    result <- run pipe $ queryP "CREATE (n:Repo {owner: {owner}, name: {name}})"
                                (fromList [("owner", T(fromString owner_name)), ("name", T (fromString repo_name))])
    result <- run pipe $ query $ Data.Text.pack $ addLink user owner_name repo_name "CONTRIBUTES"
    close pipe

addLink :: Text -> String -> String -> String -> String
addLink u o r link = "MATCH (u:User {name: '" ++ unpack u ++ "'}) MATCH (r:Repo {owner: '" ++ fromString o ++ "', name: '" ++ fromString r ++ "'}) MERGE (u)-[o:" ++ fromString link ++ "]->(r)"

-- Utility Functions
formatName :: Name a -> String
formatName = unpack . untagName

logMsg :: [String] -> IO()
logMsg = mapM_ putStr
