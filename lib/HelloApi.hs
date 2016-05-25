{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module HelloApi where

import           Data.Aeson
import           Data.Proxy
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           GHC.Generics
import           Servant.API

data User = User
    { name :: Text
    , age  :: Int
    } deriving (Eq, Show, Read, Generic)
instance FromJSON User
instance ToJSON User

type HelloAPI  = Get '[PlainText] Text
            :<|> "user" :> Capture "name" Text :> Capture "age" Int :> Get '[JSON] User

helloApi :: Proxy HelloAPI
helloApi = Proxy
