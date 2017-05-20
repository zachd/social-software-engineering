{-# LANGUAGE OverloadedStrings      #-}

module GitHubAPI where

import Data.Text
import Data.Vector

import GitHub
import GitHub.Data.Repos
import qualified GitHub.Endpoints.Users as GitHubUsers
import qualified GitHub.Endpoints.Repos as GitHubRepos
import qualified GitHub.Endpoints.Users.Followers as GitHubFollowers


formatRepo :: Repo -> Text
formatRepo = GitHubRepos.untagName . GitHub.Data.Repos.repoName

formatUser :: SimpleUser -> Text
formatUser = GitHub.untagName . GitHub.simpleUserLogin

formatContributor :: Contributor -> Text
formatContributor (KnownContributor contributions avatarUrl name url uid gravatar) = GitHub.untagName name

getUserInfo :: Text -> IO User
getUserInfo name = do
    let user = GitHub.mkUserName name
    request <- GitHubUsers.userInfoFor user
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return result

getUserRepos :: Text -> IO (Vector Text)
getUserRepos name = do
    let owner = GitHub.mkOwnerName name
    request <- GitHubRepos.userRepos owner GitHub.Data.Repos.RepoPublicityAll
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

getRepoContributors :: Text -> Text -> IO (Vector Text)
getRepoContributors user name = do
    let owner = GitHub.mkOwnerName user
    let repo = GitHub.mkRepoName name
    request <- GitHubRepos.contributors owner repo
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ Data.Vector.map formatContributor result
