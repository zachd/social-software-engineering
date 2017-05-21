{-# LANGUAGE OverloadedStrings      #-}

module GitHubAPI where

import DatabaseAPI
import Data.Text
import Data.Vector
import Data.String
import GitHub
import GitHub.Auth
import GitHub.Data
import qualified Data.ByteString.Char8 as BS
import qualified GitHub.Endpoints.Users as GitHubUsers
import qualified GitHub.Endpoints.Repos as GitHubRepos
import qualified GitHub.Endpoints.Users.Followers as GitHubFollowers

crawl_limit = 25

-- Formatting
formatUser :: SimpleUser -> String
formatUser = unpack . untagName . GitHub.simpleUserLogin

formatContributor :: Contributor -> String
formatContributor (KnownContributor contributions avatarUrl name url uid gravatar) = unpack $ untagName name

formatRepo :: Repo -> (String, String)
formatRepo repo = do
    let owner = GitHubRepos.repoOwner repo
    let owner_name = untagName $ simpleOwnerLogin owner
    let repo_name = untagName $ GitHubRepos.repoName repo
    (unpack owner_name, unpack repo_name)

-- Crawl Requests
crawlOrg :: Text -> String -> IO (Vector (String, String))
crawlOrg org token = do
    logMsg ["Crawl started for organisation: ", unpack org, "\n"]
    let auth = Just $ GitHub.Auth.OAuth $ BS.pack $ token
    repos <- getOrgRepos org auth
    result <- Data.Vector.mapM (addRepo "Org") repos
    result_two <- Data.Vector.mapM (crawlRepo auth) repos
    logMsg ["Crawl for ", unpack org, " complete!\n"]
    return repos

crawlUser :: Text -> String -> IO (Vector (String, String))
crawlUser user token = do
    logMsg ["Crawl started for user: ", unpack user, "\n"]
    let auth = Just $ GitHub.Auth.OAuth $ BS.pack $ token
    repos <- getUserRepos user auth
    result <- Data.Vector.mapM (addRepo "User") repos
    result_two <- Data.Vector.mapM (crawlRepo auth) repos
    return repos

crawlRepo :: Maybe Auth -> (String, String) -> IO (Vector String)
crawlRepo auth (owner, repo) = do
    logMsg ["Crawling repo: ", owner, "/", repo, "\n"]
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

getOrgRepos :: Text -> Maybe Auth -> IO (Vector (String, String))
getOrgRepos name auth = do
    let org = GitHub.mkOrganizationName name
    request <- GitHubRepos.organizationRepos' auth org RepoPublicityPublic
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ Data.Vector.map formatRepo (Data.Vector.take crawl_limit result)

getUserRepos :: Text -> Maybe Auth -> IO (Vector (String, String))
getUserRepos name auth = do
    let owner = GitHub.mkOwnerName name
    request <- GitHubRepos.userRepos' auth owner RepoPublicityPublic
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ Data.Vector.map formatRepo (Data.Vector.take crawl_limit result)

getRepoContributors :: (String, String) -> Maybe Auth -> IO (Vector String)
getRepoContributors (owner, repo) auth = do
    let github_owner = GitHub.mkOwnerName $ fromString owner
    let github_repo = GitHub.mkRepoName $ fromString repo
    request <- GitHubRepos.contributors' auth github_owner github_repo
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ Data.Vector.map formatContributor (Data.Vector.take crawl_limit result)
