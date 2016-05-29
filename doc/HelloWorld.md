# Hello world in Haskell with Servant

Repository
http://github.com/algas/haskell-servant-cookbook

## Create project
We assume that you have already installed haskell-stack.
Inut the following command into your terminal in order to create a servant project.

```bash
$ stack new myproject servant
```

## API
"/" returns "Hello World" and "/users/(name)/(age)" returns User.
User type has properties `name` and `age`.

```hs:lib/HelloApi.hs
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
```

## Server

```hs:server/Main.hs
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           HelloApi
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API

server :: Server HelloAPI
server = hello :<|> user
    where
        hello = return "Hello world"
        user n a = return $ User n a

app :: Application
app = serve helloApi server

main :: IO ()
main = do
    run 8080 app
```

## Client

```hs:client/Main.hs
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

hello :<|> user = client helloApi

queries :: Manager -> BaseUrl -> ExceptT ServantError IO (Text, User)
queries manager baseurl = do
    h <- hello manager baseurl
    us <- user "John Smith" 26 manager baseurl
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
```

## Check

0. Build  
`$ stack build`
0. Run Server  
`$ stack exec hello-server`
0. Test with curl  
`$ curl http://localhost:8080`  
`Hello world`  
`$ curl http://localhost:8080/users/hoge/3`  
`{"age":3,"name":"hoge"}`
0. Run client  
`$ stack exec hello-client`  
`("Hello world",User {name = "John Smith", age = 26})`

You can get values from the server with the client.
A pros of Servant is decreasing redundant description because a client and a server refer the same api. 

## Environment
- stack:lts-6.0
- servant-0.7

## Reference
http://haskell-servant.readthedocs.io/en/stable/index.html
