{-# LANGUAGE OverloadedStrings #-}
module Lib (
  handleMessage
) where

import Control.Lens

import Data.Message

import Network.Wreq

sendMessage :: Message -> IO ()
sendMessage msg = do let body = dumpMessage $ Just msg
                     r <- post "http://localhost:4040" body
                     print r

handleMessage :: Message -> IO ()
handleMessage msg = sendMessage $ msg & messageContent .~ "Hi there"
