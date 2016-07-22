{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Data.Aeson
import           Data.Aeson.Types         (typeMismatch)
import           Data.Either.Utils        (maybeToEither)
import           Data.Maybe               (fromJust, fromMaybe)
import           Data.Scientific
import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import           Data.Text.Read           (decimal, signed)
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

userList :: [User]
userList =  [ User "John Smith" (fromJust (generateTeenage 18)) (fromJust (emailAddress "john@example.com"))
            , User "Alice Jones" (fromJust (generateTeenage 14)) (fromJust (emailAddress "alice@example.com"))
            ]

instance FromHttpApiData Teenage where
    parseQueryParam x =
        let
            y = parseQueryParam x :: Either Text Integer
            in case y of
                Right r -> maybeToEither "Teenage" (generateTeenage r)
                Left l  -> Left "Teenage"

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

type StrongAPI = "users" :> QueryParam "age" Teenage :> Get '[JSON] [User]
            :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] User

strongApi :: Proxy StrongAPI
strongApi = Proxy

server :: Server StrongAPI
server = users :<|> newUser
    where
        users Nothing  = return userList
        users (Just a) = return [ u | u <- userList, age u == a ]
        newUser u = return u

app :: Application
app = serve strongApi server


main :: IO ()
main = do
    run 8080 app

