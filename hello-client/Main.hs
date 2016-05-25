{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           HelloApi
import           Network.HTTP.Client        (Manager, defaultManagerSettings,
                                             newManager)
import           Servant.API
import           Servant.Client

hello :<|> users = client helloApi

queries :: Manager -> BaseUrl -> ExceptT ServantError IO (Text, [User])
queries manager baseurl = do
    h <- hello manager baseurl
    us <- users manager baseurl
    return (h, us)

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    let baseUrl = BaseUrl Http "localhost" 8080 ""
    res <- runExceptT $ queries manager baseUrl
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right p -> do
            print p
    print ""

