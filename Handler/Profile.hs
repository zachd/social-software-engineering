module Handler.Profile where

import Import
import Data.Maybe

import GitHub
import GitHub.Data.Repos
import qualified GitHub.Endpoints.Repos as GitHubRepos
import qualified GitHub.Endpoints.Users.Followers as GitHubFollowers

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    sess <- getSession

    let login = fromJust $ lookup "login" sess
    let email = fromJust $ lookup "email" sess
    let token = fromJust $ lookup "access_token" sess

    let githubName = decodeUtf8 login
    let githubUserName = GitHub.mkUserName githubName
    let githubOwnerName = GitHub.mkOwnerName githubName

    -- Get repos for user
    reposRequest <- liftIO $ GitHubRepos.userRepos githubOwnerName GitHub.Data.Repos.RepoPublicityAll
    getRepos <- case reposRequest of
        Left e -> error $ show e
        Right res -> return res
    let repos = map formatRepo getRepos

    -- Get followers for user
    followersRequest <- liftIO $ GitHubFollowers.usersFollowing githubUserName
    getFollowers <- case followersRequest of
        Left e -> error $ show e
        Right res -> return res
    let followers = map formatUser getFollowers


    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s User page"
        $(widgetFile "profile")


formatRepo :: Repo -> Text
formatRepo = GitHubRepos.untagName . GitHub.Data.Repos.repoName

formatUser :: SimpleUser -> Text
formatUser = GitHub.untagName . GitHub.simpleUserLogin