{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude

import Lib (handleMessage)
import Data.Message (readMessage)
import Network.HTTP.Types.Status (status200, status500)
import Web.Scotty

main :: IO ()
main = scotty 3030 $
  post "/" $ do
    msgBody <- body
    case readMessage msgBody of
      Nothing -> do status status500
                    text "Dunno lol"
      Just msg -> do _ <- liftIO $ fork (handleMessage msg)
                     status status200
                     text "ok"
