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

    -- Get repos for user
    repos <- liftIO $ getRepos githubName

    -- Get followers for user
    followers <- liftIO $ getFollowers githubName

    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s User page"
        $(widgetFile "profile")


formatRepo :: Repo -> Text
formatRepo = GitHubRepos.untagName . GitHub.Data.Repos.repoName

formatUser :: SimpleUser -> Text
formatUser = GitHub.untagName . GitHub.simpleUserLogin

getRepos :: Text -> IO (Vector Text)
getRepos name = do
    let owner = GitHub.mkOwnerName name
    request <- liftIO $ GitHubRepos.userRepos owner GitHub.Data.Repos.RepoPublicityAll
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ map formatRepo result

getFollowers :: Text -> IO (Vector Text)
getFollowers name = do
    let user = GitHub.mkUserName name
    request <- liftIO $ GitHubFollowers.usersFollowing user
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ map formatUser result
