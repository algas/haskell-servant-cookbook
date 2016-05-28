{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text                   (Text)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Servant
import           Servant.API
import           SimpleApi

main :: IO ()
main = do
    runTLS (tlsSettings "server.crt" "server.key") (setPort 8080 defaultSettings) app
