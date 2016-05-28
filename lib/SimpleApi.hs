{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module SimpleApi where

import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Data.Aeson
import           Data.Proxy
import           Data.Text                  (Text)
import           GHC.Generics
import           Network.HTTP.Client        (Manager)
import           Network.Wai
import           Servant
import           Servant.API
import           Servant.Client


type SimpleAPI  = Get '[PlainText] Text

simpleApi :: Proxy SimpleAPI
simpleApi = Proxy

simple = client simpleApi

queries :: Manager -> BaseUrl -> ExceptT ServantError IO Text
queries manager baseurl = do
    h <- simple manager baseurl
    return h

server :: Server SimpleAPI
server = return "Simple"

app :: Application
app = serve simpleApi server

