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
import           SimpleApi


main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    let baseUrl = BaseUrl Https "localhost" 8080 ""
    res <- queries manager baseUrl
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right p -> do
            print p
