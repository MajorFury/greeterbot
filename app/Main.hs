{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude

import Lib (readMessage)
import Network.HTTP.Types.Status (status500)
import Web.Scotty

main :: IO ()
main = scotty 3030 $
  post "/" $ do
    msg <- body
    case readMessage msg of
      Left _ -> do status status500
                   text "Dunno lol"
      Right content -> do setHeader "Content-Type" "application/transit+msgpack"
                          raw content
