module Handler.Profile where

import Import
import Data.Maybe

import GitHub.Data.Repos
import qualified GitHub
import qualified GitHub.Endpoints.Repos as GitHubRepos

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    sess <- getSession

    let login = lookup "login" sess
    let email = lookup "email" sess
    let token = lookup "access_token" sess

    let githubName = decodeUtf8 $ fromJust login
    let githubOwnerName = GitHub.mkOwnerName githubName

    possibleRepos <- liftIO $ GitHubRepos.userRepos githubOwnerName GitHub.Data.Repos.RepoPublicityAll
    repos <- case possibleRepos of
        Left e -> error $ show e
        Right repos -> return repos



    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s User page"
        $(widgetFile "profile")

formatRepo :: Repo -> Text
formatRepo repo = GitHubRepos.untagName $ GitHub.Data.Repos.repoName repo
