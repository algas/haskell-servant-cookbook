# Logging for Haskell Servant with wai-logger

Servant make no log output with default settings when you access your server.
The fact makes it very difficult for you to fix bugs.
To make easier to debug, you should use logger.

We will introduce how to install logger into your servant server.

Repository https://github.com/algas/haskell-servant-cookbook

## Logging

Normal function to `run`.

```hs
-- run :: Port -> Application -> IO ()
run 8080 app
```

Now we use `runSettings` (Network.Wai.Handler.Warp) with `withStdoutLogger` (Network.Wai.Logger).

```hs
-- withStdoutLogger :: (ApacheLogger -> IO a) -> IO a
-- setLogger :: (Request -> Status -> Maybe Integer -> IO ()) -> Settings -> Settings
-- type ApacheLogger = Request -> Status -> Maybe Integer -> IO ()
-- runSettings :: Settings -> Application -> IO ()
main = do
    withStdoutLogger $ \aplogger -> do
        let settings = setPort 8080 $ setLogger aplogger defaultSettings
        runSettings settings app
```

## Usage

```bash
$ stack exec logger
127.0.0.1 - - [25/May/2016:16:20:01 +0900] "GET / HTTP/1.1" 200 - "" "curl/7.43.0"
```

```bash
$ curl http://localhost:8080
```

You can confirm that logs in stdout when you connect the server from a http client like curl. 

## Source
```hs:logger/Main.hs
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Data.Text                (Text)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger       (withStdoutLogger)
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
    withStdoutLogger $ \aplogger -> do
        let settings = setPort 8080 $ setLogger aplogger defaultSettings
        runSettings settings app
```
```hs:haskell-servant-cookbook.cabal
executable logger
  hs-source-dirs:      logger
  main-is:             Main.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N
  build-depends:       base
                     , text
                     , aeson
                     , wai
                     , wai-logger
                     , warp
                     , servant
                     , servant-server
                     , haskell-servant-cookbook
  default-language:    Haskell2010
```

## Reference

http://www.parsonsmatt.org/2015/05/02/scotty_and_persistent.html
http://qiita.com/lotz/items/c357a41d4432942d8054
