{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text                  (Text)
import           HelloApi
import           Network.HTTP.Client        (Manager, defaultManagerSettings,
                                             newManager)
import           Servant.API
import           Servant.Client

hello :<|> user = client helloApi

queries :: Manager -> BaseUrl -> IO (Either ServantError (Text, User))
queries manager baseurl = do
    h <- runClientM hello $ mkClientEnv manager baseurl
    us <- runClientM (user "John Smith" 26) $ mkClientEnv manager baseurl
    return $ (,) <$> h <*> us

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    let baseUrl = BaseUrl Http "localhost" 8080 ""
    res <- queries manager baseUrl
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right p -> do
            print p
