{-# LANGUAGE OverloadedStrings      #-}

module GitHubAPI where

import DatabaseAPI
import Data.Text
import Data.Vector
import GitHub
import GitHub.Data
import qualified GitHub.Endpoints.Users as GitHubUsers
import qualified GitHub.Endpoints.Repos as GitHubRepos
import qualified GitHub.Endpoints.Users.Followers as GitHubFollowers

-- Formatting
formatUser :: SimpleUser -> Text
formatUser = untagName . GitHub.simpleUserLogin

formatContributor :: Contributor -> Text
formatContributor (KnownContributor contributions avatarUrl name url uid gravatar) = GitHub.untagName name

formatRepo :: Repo -> (Text, Text)
formatRepo repo = do
    let owner = GitHubRepos.repoOwner repo
    let owner_name = untagName $ simpleOwnerLogin owner
    let repo_name = untagName $ GitHubRepos.repoName repo
    (owner_name, repo_name)

-- Begin Crawl
crawlUser :: Text -> IO (Vector (Text, Text))
crawlUser user = do
    logMsg ["Crawl started from user: ", unpack user, "\n"]
    repos <- getUserRepos user
    result <- Data.Vector.mapM addRepo repos
    result_two <- Data.Vector.mapM crawlRepo repos
    return repos

crawlRepo :: (Text, Text) -> IO (Vector Contributor)
crawlRepo (owner, repo) = do
    logMsg ["Crawl started from repo: ", unpack owner, "/", unpack repo, "\n"]
    contributors <- getRepoContributors owner repo
    --result <- Data.Vector.mapM addContributor contributors
    return contributors

-- API Requests
getUserInfo :: Text -> IO User
getUserInfo name = do
    let user = GitHub.mkUserName name
    request <- GitHubUsers.userInfoFor user
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return result

getUserRepos :: Text -> IO (Vector (Text, Text))
getUserRepos name = do
    let owner = GitHub.mkOwnerName name
    request <- GitHubRepos.userRepos owner RepoPublicityAll
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ Data.Vector.map formatRepo result

getUserFollowers :: Text -> IO (Vector Text)
getUserFollowers name = do
    let user = GitHub.mkUserName name
    request <- GitHubFollowers.usersFollowing user
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ Data.Vector.map formatUser result

getRepoContributors :: Text -> Text -> IO (Vector Contributor)
getRepoContributors owner repo = do
    let github_owner = GitHub.mkOwnerName owner
    let github_repo = GitHub.mkRepoName repo
    request <- GitHubRepos.contributors github_owner github_repo
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ result
