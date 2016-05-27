{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text                   (Text)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Servant
import           Servant.API

type SimpleAPI  = Get '[PlainText] Text

simpleApi :: Proxy SimpleAPI
simpleApi = Proxy

server :: Server SimpleAPI
server = return "Simple"

app :: Application
app = serve simpleApi server
main :: IO ()
main = do
    runTLS (tlsSettings "server.crt" "server.key") (setPort 8080 defaultSettings) app
