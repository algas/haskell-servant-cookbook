{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Data.Text                (Text)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger       (withStdoutLogger)
import           Servant
import           Servant.API

type SimpleAPI  = "log" :> Get '[PlainText] Text

simpleApi :: Proxy SimpleAPI
simpleApi = Proxy

server :: Server SimpleAPI
server = return "Simple"

app :: Application
app = serve simpleApi server

main :: IO ()
main = do
    withStdoutLogger $ \aplogger -> do
        let settings = setPort 8080 $ setLogger aplogger defaultSettings
        runSettings settings app
