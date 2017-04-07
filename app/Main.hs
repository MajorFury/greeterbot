{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text.Lazy (pack)
import Lib (readMessage)
import Web.Scotty

main :: IO ()
main = scotty 3030 $
  post "/" $ do
    msg <- body
    text $ pack . show . readMessage $ msg
