# Testing in Haskell with Servant

Repository
http://github.com/algas/haskell-servant-cookbook

## Test target

The following API returns "Simple" when you send a request to your server.

```hs:lib/SimpleApi.hs
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module SimpleApi where

import           Data.Aeson
import           Data.Proxy
import           Data.Text                  (Text)
import           GHC.Generics
import           Network.Wai
import           Servant
import           Servant.API

type SimpleAPI  = Get '[PlainText] Text

simpleApi :: Proxy SimpleAPI
simpleApi = Proxy

server :: Server SimpleAPI
server = return "Simple"

app :: Application
app = serve simpleApi server
```

## Test code

Use `with` function in hspec-wai to call `app` function in `SimpleApi`

```hs
with :: IO a -> SpecWith a -> Spec
```

Test codes as following.

```hs:test/Spec.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import           SimpleApi           (app)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
    describe "GET /" $ do
        it "responds with 200" $ do
            get "/" `shouldRespondWith` 200
        it "responds with 'Simple'" $ do
            get "/" `shouldRespondWith` "Simple"
```

```hs:haskell-servant-cookbook.cabal
test-suite test
  type:                exitcode-stdio-1.0
  hs-source-dirs:      test
  main-is:             Spec.hs
  build-depends:       base
                     , hspec
                     , hspec-wai
                     , hspec-wai-json
                     , aeson
                     , transformers
                     , haskell-servant-cookbook
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N
  default-language:    Haskell2010
```


## Usage

```bash
$ stack test
```
```bash
haskell-servant-cookbook-0.1.0.0: test (suite: test)

Progress: 1/2
GET /
  responds with 200
  responds with 'Simple'

Finished in 0.0138 seconds
2 examples, 0 failures

Completed 2 action(s).
```

## Reference

https://github.com/commercialhaskell/stack-templates/blob/master/scotty-hspec-wai.hsfiles
