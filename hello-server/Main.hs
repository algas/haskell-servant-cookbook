{-# LANGUAGE OverloadedStrings #-}
module Main where

import           HelloApi
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API

server :: Server HelloAPI
server = hello :<|> users
    where
        hello = return "Hello world"
        users = return [User "John Smith" 26]

app :: Application
app = serve helloApi server

main :: IO ()
main = do
    run 8080 app
