{-# LANGUAGE OverloadedStrings #-}
module Lib (
    handleMessage
  , Config
) where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Message
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Network.Wreq

data Config = Config { braidUrl :: String
                     , botId    :: ByteString
                     , botToken :: ByteString }
              deriving (Show)

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
      v .: "braidUrl" <*>
      (encodeUtf8 <$> v .: "botId")    <*>
      (encodeUtf8 <$> v .: "botToken")
  parseJSON _ = fail "malformed config"

sendMessage :: Config -> Message -> IO ()
sendMessage conf msg = do let body = dumpMessage $ Just msg
                          let opts = defaults & auth ?~ basicAuth (botId conf) (botToken conf)
                                              & header "Content-Type" .~ ["application/transit+msgpack"]
                          r <- postWith opts ((braidUrl conf) ++ "/bots/message") body
                          print r

handleMessage :: Config -> Message -> IO ()
handleMessage conf msg = sendMessage conf $ msg & messageContent .~ "Hi there"
