# Https in Haskell with Servant

How to use https in your client and server with servant.

Repository
http://github.com/algas/haskell-servant-cookbook

## Client

Use `tlsManagerSettings` in http-client-tls instead of `defaultManagerSettings`.

## Client

```hs:https-client/Main.hs
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

type SimpleAPI  = Get '[PlainText] Text

simpleApi :: Proxy SimpleAPI
simpleApi = Proxy

simple = client simpleApi

queries :: Manager -> BaseUrl -> ExceptT ServantError IO (Text)
queries manager baseurl = do
    h <- simple manager baseurl
    return h

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    let baseUrl = BaseUrl Https "localhost" 8080 ""
    res <- runExceptT $ queries manager baseUrl
    case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right p -> do
            print p
```

## Server

Use `runTLS`, `tlsSettings` in warp-tls library.

```hs:https-server/Main.hs
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text                   (Text)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Servant
import           Servant.API

type SimpleAPI  = Get '[PlainText] Text

simpleApi :: Proxy SimpleAPI
simpleApi = Proxy

server :: Server SimpleAPI
server = return "Simple"

app :: Application
app = serve simpleApi server
main :: IO ()
main = do
    runTLS (tlsSettings "server.crt" "server.key") (setPort 8080 defaultSettings) app
```

## Dependencies

```hs:haskell-servant-cookbook.cabal
executable https-client
  hs-source-dirs:      https-client
  main-is:             Main.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N
  build-depends:       base
                     , text
                     , aeson
                     , http-client
                     , http-client-tls
                     , transformers
                     , servant
                     , servant-client
                     , haskell-servant-cookbook
  default-language:    Haskell2010

executable https-server
  hs-source-dirs:      https-server
  main-is:             Main.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N
  build-depends:       base
                     , text
                     , aeson
                     , wai
                     , warp
                     , warp-tls
                     , servant
                     , servant-server
                     , haskell-servant-cookbook
  default-language:    Haskell2010
```

## Reference
https://hackage.haskell.org/package/scotty-tls  
http://d.hatena.ne.jp/ozuma/20130511/1368284304
