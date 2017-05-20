{-# LANGUAGE OverloadedStrings      #-}

module GitHubAPI where

import DatabaseAPI
import Data.Text
import Data.Vector
import GitHub
import GitHub.Auth
import GitHub.Data
import qualified Data.ByteString.Char8 as BS
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

-- Crawl Requests
crawlOrg :: Text -> String -> IO (Vector (Text, Text))
crawlOrg org token = do
    logMsg ["Crawl started for organisation: ", unpack org, "\n"]
    let auth = Just $ GitHub.Auth.OAuth $ BS.pack $ token
    repos <- getOrgRepos org auth
    result <- Data.Vector.mapM addRepo repos
    result_two <- Data.Vector.mapM (crawlRepo auth) repos
    return repos
    
crawlUser :: Text -> String -> IO (Vector (Text, Text))
crawlUser user token = do
    logMsg ["Crawl started for user: ", unpack user, "\n"]
    let auth = Just $ GitHub.Auth.OAuth $ BS.pack $ token
    repos <- getUserRepos user auth
    result <- Data.Vector.mapM addRepo repos
    result_two <- Data.Vector.mapM (crawlRepo auth) repos
    return repos

crawlRepo :: Maybe Auth -> (Text, Text) -> IO (Vector Text)
crawlRepo auth (owner, repo) = do
    logMsg ["Crawling repo: ", unpack owner, "/", unpack repo, "\n"]
    contributors <- getRepoContributors (owner, repo) auth
    result <- Data.Vector.mapM (addContributor (owner, repo)) contributors
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

getOrgRepos :: Text -> Maybe Auth -> IO (Vector (Text, Text))
getOrgRepos name auth = do
    let org = GitHub.mkOrganizationName name
    request <- GitHubRepos.organizationRepos' auth org RepoPublicityPublic
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ Data.Vector.map formatRepo result

getUserRepos :: Text -> Maybe Auth -> IO (Vector (Text, Text))
getUserRepos name auth = do
    let owner = GitHub.mkOwnerName name
    request <- GitHubRepos.userRepos' auth owner RepoPublicityPublic
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ Data.Vector.map formatRepo result

getRepoContributors :: (Text, Text) -> Maybe Auth -> IO (Vector Text)
getRepoContributors (owner, repo) auth = do
    let github_owner = GitHub.mkOwnerName owner
    let github_repo = GitHub.mkRepoName repo
    request <- GitHubRepos.contributors' auth github_owner github_repo
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ Data.Vector.map formatContributor result
