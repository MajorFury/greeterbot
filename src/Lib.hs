{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( readMessage
    ) where

import Control.Applicative ( (<$>) )
import Control.Lens

import Data.Serialize (decodeLazy, encodeLazy)
import Data.Text
import Data.Transit
import Data.UUID
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Data.MessagePack as MP

data Message = Message {
    _messageId             :: UUID
  , _messageContent        :: Text
  , _messageGroupId        :: UUID
  , _messageMentionedTags  :: [UUID]
  , _messageMentionedUsers :: [UUID]
  , _messageUserId         :: UUID
  , _messageThreadId       :: UUID
} deriving (Show)
makeLenses ''Message

unwrapUUID :: Transit -> Maybe UUID
unwrapUUID (TransitUUID u) = Just u
unwrapUUID _               = Nothing

unwrapUUIDs :: [Transit] -> Maybe [UUID]
unwrapUUIDs = sequence . fmap unwrapUUID

messageFromMap :: M.Map Transit Transit -> Maybe Message
messageFromMap m = let lookup' k = M.lookup (TransitKeyword k) m in
  do
    TransitUUID id' <- lookup' "id"
    TransitUUID user <- lookup' "user-id"
    TransitUUID thread <- lookup' "thread-id"
    TransitString content <- lookup' "content"
    TransitUUID groupId <- lookup' "group-id"
    TransitArray tags <- lookup' "mentioned-tag-ids"
    tags' <- unwrapUUIDs tags
    TransitArray users <- lookup' "mentioned-user-ids"
    users' <- unwrapUUIDs users
    return Message { _messageId = id'
                   , _messageUserId = user
                   , _messageThreadId = thread
                   , _messageContent = content
                   , _messageGroupId = groupId
                   , _messageMentionedTags = tags'
                   , _messageMentionedUsers = users' }

messageToMap :: Message -> Transit
messageToMap m = TransitMap $ M.fromList [
      (TransitKeyword "id", TransitUUID (m ^. messageId))
   , (TransitKeyword "group-id", TransitUUID (m ^. messageGroupId))
   , (TransitKeyword "thread-id", TransitUUID (m ^. messageThreadId))
   , (TransitKeyword "user-id", TransitUUID (m ^. messageUserId))
   , (TransitKeyword "content", TransitString (m ^. messageContent))
   , (TransitKeyword "mentioned-tag-ids", TransitArray $ TransitUUID <$> m ^. messageMentionedTags)
   , (TransitKeyword "mentioned-user-ids", TransitArray $ TransitUUID <$> m ^. messageMentionedUsers)
  ]

instance Transitable (Maybe Message) where
  fromTransit (TransitMap m) = messageFromMap m
  fromTransit _              = Nothing

  toTransit (Just msg) = messageToMap msg
  toTransit Nothing    = TransitNil

handleMessage :: Message -> Message
handleMessage msg = msg & messageContent .~ "Hello from Haskell"

toMessagePack :: Maybe Message -> MP.Object
toMessagePack = fromTransit . toTransit

dumpMessage :: Maybe Message -> BL.ByteString
dumpMessage = encodeLazy . toMessagePack

readMessage :: BL.ByteString -> Either String BL.ByteString
readMessage msg = do
  obj <- decodeLazy msg
  let transit = toTransit (obj :: MP.Object)
  return $ dumpMessage (fromTransit transit >>= (Just . handleMessage))
