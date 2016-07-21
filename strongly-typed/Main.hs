{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Data.Aeson
import           Data.Aeson.Types         (typeMismatch)
import           Data.Maybe               (fromJust)
import           Data.Scientific
import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API
import           Teenage
import           Text.Email.Parser
import           Text.Email.Validate


data User = User
    { name  :: Text
    , age   :: Teenage
    , email :: EmailAddress
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

user1 :: User
user1 = User "John Smith" (fromJust (generateTeenage 18)) (fromJust (emailAddress "foo@example.com"))

instance FromJSON Teenage where
    parseJSON (Number s) =
        case generateTeenage (coefficient s) of
            Just n -> return n
            _      -> typeMismatch "Teenage" (Number s)
    parseJSON m = typeMismatch "Teenage" m

instance ToJSON Teenage where
    toJSON = Number . flip scientific 0 . teenage

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
        users = return [user1]
        newUser u = return u

app :: Application
app = serve strongApi server


main :: IO ()
main = do
    run 8080 app

