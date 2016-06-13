{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Data.Aeson
import           Data.ByteString                  (ByteString)
import           Data.Map                         (Map)
import qualified Data.Map                         as M
import           Data.Maybe                       (fromJust, fromMaybe)
import           Data.Monoid                      ((<>))
import           Data.Text                        (Text)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API
import           Servant.API.Experimental.Auth    (AuthProtect)
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)
import           Servant.Server.Experimental.Auth ()


newtype PrivateData = PrivateData { ssshhh :: Text }
    deriving (Eq, Show, Generic)
instance ToJSON PrivateData

newtype PublicData = PublicData { somedata :: Text }
    deriving (Eq, Show, Generic)
instance ToJSON PublicData

newtype User = User { userName :: Text }
    deriving (Eq, Show)

type PublicAPI = Get '[JSON] [PublicData]
type PrivateAPI = Get '[JSON] PrivateData

newtype Account = Account { unAccount :: Text }

database :: Map ByteString Account
database = M.fromList [ ("key1", Account "Anne Briggs")
                      , ("key2", Account "Bruce Cockburn")
                      , ("key3", Account "Ghédalia Tazartès")
                      ]

lookupAccount :: ByteString -> Handler Account
lookupAccount key = case M.lookup key database of
    Nothing -> throwError (err403 { errBody = "Invalid Cookie" })
    Just usr -> return usr

authHandler :: AuthHandler Request Account
authHandler =
    let handler req = case lookup "servant-auth-cookie" (requestHeaders req) of
            Nothing -> throwError (err401 { errBody = "Missing auth header" })
            Just authCookieKey -> lookupAccount authCookieKey
        in mkAuthHandler handler

type AuthGenAPI = "private" :> AuthProtect "cookie-auth" :> PrivateAPI
             :<|> "public"  :> PublicAPI

genAuthAPI :: Proxy AuthGenAPI
genAuthAPI = Proxy

type instance AuthServerData (AuthProtect "cookie-auth") = Account

genAuthServerContext :: Context (AuthHandler Request Account ': '[])
genAuthServerContext = authHandler :. EmptyContext

genAuthServer :: Server AuthGenAPI
genAuthServer =
    let privateDataFunc (Account name) = return (PrivateData ("this is a secret: " <> name))
        publicData = return [PublicData "this is a public piece of data"]
    in  privateDataFunc :<|> publicData

genAuthMain :: IO ()
genAuthMain = run 8080 (serveWithContext genAuthAPI genAuthServerContext genAuthServer)


main :: IO ()
main = do
    genAuthMain
