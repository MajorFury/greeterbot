{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Prelude

import Crypto.MAC.HMAC (hmac, hmacGetDigest)
import Crypto.Hash (Digest)
import Crypto.Hash.Algorithms (SHA256)
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as TL
import Data.Event (readEvent)
import Data.Message (readMessage)
import qualified Data.Yaml as Y
import Lib (Config, botToken, handleEvent, handleMessage)
import Network.HTTP.Types.Status (status200, status400, status500)
import Web.Scotty

verifyHMAC :: Maybe B.ByteString -> B.ByteString -> B.ByteString -> Bool
verifyHMAC Nothing _ _ = False
verifyHMAC (Just givenMac) macKey msgBody =
  givenMac == encodeUtf8 (tshow (hmacGetDigest $ hmac macKey msgBody :: Digest SHA256))

main :: IO ()
main = do
  conf <- Y.decodeFile "conf.yaml"
  case conf :: Maybe Config of
    Nothing -> print ("Couldn't load config"::Text)
    Just c -> do
      scotty 3030 $ do
        post "/event" $ do
          msgBody <- body
          requestMac <- header "X-Braid-Signature"
          if verifyHMAC (fmap (encodeUtf8 . TL.toStrict) requestMac) (botToken c) (toStrict msgBody) then
            case readEvent msgBody of
              Nothing -> do status status500
                            text "Dunno lol"
              Just evt -> do _ <- liftIO $ fork (handleEvent c evt)
                             status status200
                             text "ok"
          else
            do status status400
               text "Bad hmac"
          status status200
          text "ok"
        put "/message" $ do
          requestMac <- header "X-Braid-Signature"
          msgBody <- body
          if verifyHMAC (fmap (encodeUtf8 . TL.toStrict) requestMac) (botToken c) (toStrict msgBody) then
            case readMessage msgBody of
              Nothing -> do status status500
                            text "Dunno lol"
              Just msg -> do _ <- liftIO $ fork (handleMessage c msg)
                             status status200
                             text "ok"
          else
            do status status400
               text "Bad hmac"
