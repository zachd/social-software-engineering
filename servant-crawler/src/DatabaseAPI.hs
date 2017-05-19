{-# LANGUAGE OverloadedStrings      #-}

module DatabaseAPI where

import Database.Bolt
import Data.String
import Data.Text
import Data.Map

config :: BoltCfg
config = def { user = "neo4j", password = "neo4j"}


-- CREATE (n:Person { name: 'Andres', title: 'Developer' })

addUser :: String -> IO()
addUser user = do
    pipe <- connect config
    result <- run pipe $ queryP "CREATE (n:User {name: {name}})" 
                              (fromList [("name", T (fromString user))])
    putStrLn $ show result
    close pipe