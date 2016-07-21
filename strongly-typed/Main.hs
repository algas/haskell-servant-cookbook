{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Data.Aeson
import           Data.Aeson.Types         (typeMismatch)
import           Data.Maybe               (fromJust)
import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API
import           Text.Email.Parser
import           Text.Email.Validate

data User = User
    { name  :: Text
    , email :: EmailAddress
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance FromJSON EmailAddress where
    parseJSON (String s) =
        case emailAddress (encodeUtf8 s) of
            Just e -> return e
            _      -> typeMismatch "EmailAddress" (String s)
    parseJSON m = typeMismatch "EmailAddress" m

instance ToJSON EmailAddress where
    toJSON = String . decodeUtf8 . toByteString

type StrongAPI = "users" :> Get '[JSON] [User]
            :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User

strongApi :: Proxy StrongAPI
strongApi = Proxy

server :: Server StrongAPI
server = users :<|> newUser
    where
        users = return [User "John Smith" (fromJust (emailAddress "foo@example.com"))]
        newUser u = return u

app :: Application
app = serve strongApi server


main :: IO ()
main = do
    run 8080 app

