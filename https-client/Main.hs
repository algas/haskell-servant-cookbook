{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Data.Proxy
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           HelloApi
import           Network.HTTP.Client        (Manager, newManager)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Servant.API
import           Servant.Client

type SimpleAPI  = Get '[PlainText] Text

simpleApi :: Proxy SimpleAPI
simpleApi = Proxy

simple = client simpleApi

queries :: Manager -> BaseUrl -> ExceptT ServantError IO (Text)
queries manager baseurl = do
    h <- simple manager baseurl
    return h

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    let baseUrl = BaseUrl Https "localhost" 8080 ""
    res <- runExceptT $ queries manager baseUrl
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right p -> do
            print p
