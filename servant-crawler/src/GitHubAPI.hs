module GitHubAPI where

import Data.Text
import Data.Vector

import GitHub
import GitHub.Data.Repos
import qualified GitHub.Endpoints.Repos as GitHubRepos
import qualified GitHub.Endpoints.Users.Followers as GitHubFollowers


formatRepo :: Repo -> Text
formatRepo = GitHubRepos.untagName . GitHub.Data.Repos.repoName

formatUser :: SimpleUser -> Text
formatUser = GitHub.untagName . GitHub.simpleUserLogin

getRepos :: Text -> IO (Vector Text)
getRepos name = do
    let owner = GitHub.mkOwnerName name
    request <- GitHubRepos.userRepos owner GitHub.Data.Repos.RepoPublicityAll
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ Data.Vector.map formatRepo result

getFollowers :: Text -> IO (Vector Text)
getFollowers name = do
    let user = GitHub.mkUserName name
    request <- GitHubFollowers.usersFollowing user
    result <- case request of
        Left e -> error $ show e
        Right res -> return res
    return $ Data.Vector.map formatUser result
