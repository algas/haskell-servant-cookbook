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
