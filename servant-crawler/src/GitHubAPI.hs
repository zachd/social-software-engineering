{-# LANGUAGE OverloadedStrings      #-}

module GitHubAPI where

import DatabaseAPI
import Data.Text
import Data.Vector
import GitHub
import GitHub.Data.Repos
import qualified GitHub.Endpoints.Users as GitHubUsers
import qualified GitHub.Endpoints.Repos as GitHubRepos
import qualified GitHub.Endpoints.Users.Followers as GitHubFollowers

-- Formatting
formatRepo :: Repo -> String
formatRepo = formatName . GitHubRepos.repoName

formatUser :: SimpleUser -> Text
formatUser = untagName . GitHub.simpleUserLogin

formatContributor :: Contributor -> Text
formatContributor (KnownContributor contributions avatarUrl name url uid gravatar) = GitHub.untagName name


-- Begin Crawl
crawlUser :: Text -> IO (Vector Repo)
crawlUser user = do
    logMsg ["Crawl started from user: ", unpack user, "\n"]
    repos <- getUserRepos user
    result <- Data.Vector.mapM (addRepo user) repos
    return repos


-- API Requests
getUserInfo :: Text -> IO User
getUserInfo name = do
    let user = GitHub.mkUserName name
    request <- GitHubUsers.userInfoFor user
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return result

getUserRepos :: Text -> IO (Vector Repo)
getUserRepos name = do
    let owner = GitHub.mkOwnerName name
    request <- GitHubRepos.userRepos owner GitHub.Data.Repos.RepoPublicityAll
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ result

getUserFollowers :: Text -> IO (Vector Text)
getUserFollowers name = do
    let user = GitHub.mkUserName name
    request <- GitHubFollowers.usersFollowing user
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ Data.Vector.map formatUser result

getRepoContributors :: Text -> Text -> IO (Vector Text)
getRepoContributors user name = do
    let owner = GitHub.mkOwnerName user
    let repo = GitHub.mkRepoName name
    request <- GitHubRepos.contributors owner repo
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ Data.Vector.map formatContributor result
