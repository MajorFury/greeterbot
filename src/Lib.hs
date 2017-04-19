{-# LANGUAGE OverloadedStrings #-}
module Lib (
    handleMessage
  , handleEvent
  , Config(..)
) where

import Prelude
import Control.Lens
import Db
import Data.ByteString (ByteString)
import Data.Event
import Data.Message
import qualified Data.UUID as U
import qualified Data.UUID.V4 as UUID
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Network.Wreq

data Config = Config { braidUrl :: String
                     , botId    :: ByteString
                     , botToken :: ByteString }
              deriving (Show)

instance FromJSON Config where
  parseJSON (Y.Object v) = Config <$>
                            v .: "braidUrl" <*>
                            (encodeUtf8 <$> v .: "botId") <*>
                            (encodeUtf8 <$> v .: "botToken")
  parseJSON _ = fail "malformed config"

sendMessage :: Config -> Message -> IO ()
sendMessage conf msg = do let body = dumpMessage $ Just msg
                          let opts = defaults & auth ?~ basicAuth (botId conf) (botToken conf)
                                              & header "Content-Type" .~ ["application/transit+msgpack"]
                          r <- postWith opts (braidUrl conf ++ "/bots/message") body
                          print r

-- Responding to messages

responseTo :: Message -> IO Message
responseTo m = do msgId <- UUID.nextRandom
                  return Message { _messageContent = fold ["Nice, @"
                                                          , m ^. messageUserId . to U.toText
                                                          , " you sent your first public message!"]
                                 , _messageId = msgId
                                 , _messageGroupId = m ^. messageGroupId
                                 , _messageUserId = U.nil
                                 , _messageThreadId = m ^. messageThreadId
                                 , _messageMentionedTags = []
                                 , _messageMentionedUsers = [m ^. messageUserId]
                                 }

handleMessage :: Config -> Message -> IO ()
handleMessage conf msg = do saw <- didSeeUser $ msg ^. messageUserId
                            unless saw $ do
                              sawUser $ msg ^. messageUserId
                              toSend <- responseTo msg
                              sendMessage conf toSend

-- Responding to events

welcomeMessageContent :: UserId -> Text
welcomeMessageContent userId = fold ["Welcome to Braid, @" , U.toText userId , "!\n"
                                    , "I am the greeter bot!\n"
                                    , "It's kind of quiet right now, but someone will be here later!"]

newMessage :: UserId -> GroupId -> IO Message
newMessage userId groupId = do msgId <- UUID.nextRandom
                               threadId <- UUID.nextRandom
                               return Message { _messageContent = ""
                                              , _messageId = msgId
                                              , _messageGroupId = groupId
                                              , _messageUserId = U.nil
                                              , _messageThreadId = threadId
                                              , _messageMentionedTags = []
                                              , _messageMentionedUsers = [userId]
                                              }

howToMessageContent :: Text
howToMessageContent = fold ["Use the \"#\" character to begin autocompleting tags and \"@\" to mention other users.\n"
                           ,"You can autocomplete emoji by typing \":\" and typing out the name - try typing \":smi\"!\n"
                           ]

handleEvent ::  Config -> Event -> IO ()
handleEvent conf (NewUser grp user) = do print ("New User"::Text)
                                         hiMsg <- newMessage user grp
                                         let hiMsg' = hiMsg & messageContent .~ welcomeMessageContent user
                                         sendMessage conf hiMsg'
                                         howToMsg <- newMessage user grp
                                         let howToMsg' = howToMsg & messageThreadId .~ (hiMsg ^. messageThreadId)
                                         let howToMsg'' = howToMsg' & messageContent .~ howToMessageContent
                                         sendMessage conf howToMsg''
handleEvent _ (NewAdmin grp user) = do print ("New admin"::Text)
                                       print grp
                                       print user
handleEvent _ (UserLeft grp user) = do print ("User left"::Text)
                                       print grp
                                       print user
handleEvent _ (Other t) = print t
