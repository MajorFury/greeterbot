{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Prelude

import Data.Message (readMessage)
import qualified Data.Yaml as Y
import Lib (Config, handleMessage)
import Network.HTTP.Types.Status (status200, status500)
import Web.Scotty

main :: IO ()
main = do
  conf <- Y.decodeFile "conf.yaml"
  case conf :: Maybe Config of
    Nothing -> print ("Couldn't load config"::Text)
    Just c -> do
      scotty 3030 $
        post "/" $ do
          msgBody <- body
          case readMessage msgBody of
            Nothing -> do status status500
                          text "Dunno lol"
            Just msg -> do _ <- liftIO $ fork (handleMessage c msg)
                           status status200
                           text "ok"
