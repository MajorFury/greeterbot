{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Message
  (   readMessage
    , dumpMessage
    , Message(..)
    , messageId
    , messageContent
    , messageGroupId
    , messageMentionedTags
    , messageMentionedUsers
    , messageThreadId
    , messageUserId
  ) where

import Prelude

import Control.Applicative ( (<$>) )
import Control.Lens
import Control.Lens.Fold ()
import Control.Lens.Prism ()

import Data.Serialize (decodeLazy, encode)
import Data.Transit
import Data.UUID
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Data.Map.Lens ()
import qualified Data.MessagePack as MP

data Message = Message {
    _messageId             :: UUID
  , _messageGroupId        :: UUID
  , _messageUserId         :: UUID
  , _messageThreadId       :: UUID
  , _messageContent        :: Text
  , _messageMentionedTags  :: [UUID]
  , _messageMentionedUsers :: [UUID]
} deriving (Show)
makeLenses ''Message

messageFromMap :: M.Map Transit Transit -> Maybe Message
messageFromMap m = let lookup' k = at (TransitKeyword k) . _Just in
  Message <$>
    (m ^? lookup' "id" . _TransitUUID) <*>
    (m ^? lookup' "group-id" . _TransitUUID) <*>
    (m ^? lookup' "user-id" . _TransitUUID) <*>
    (m ^? lookup' "thread-id" . _TransitUUID) <*>
    (m ^? lookup' "content" . _TransitString) <*>
    Just (m ^.. lookup' "mentioned-tag-ids" . _TransitArray . each . _TransitUUID) <*>
    Just (m ^.. lookup' "mentioned-user-ids" . _TransitArray . each . _TransitUUID)

messageToMap :: Message -> Transit
messageToMap Message{..} = TransitMap $ M.fromList [
     (TransitKeyword "id"                 ,  TransitUUID _messageId)
   , (TransitKeyword "group-id"           ,  TransitUUID _messageGroupId)
   , (TransitKeyword "thread-id"          ,  TransitUUID _messageThreadId)
   , (TransitKeyword "user-id"            ,  TransitUUID _messageUserId)
   , (TransitKeyword "content"            ,  TransitString _messageContent)
   , (TransitKeyword "mentioned-tag-ids"  ,  TransitArray $ TransitUUID <$> _messageMentionedTags)
   , (TransitKeyword "mentioned-user-ids" ,  TransitArray $ TransitUUID <$> _messageMentionedUsers)
  ]

instance Transitable (Maybe Message) where
  fromTransit (TransitMap m) = messageFromMap m
  fromTransit _              = Nothing

  toTransit (Just msg) = messageToMap msg
  toTransit Nothing    = TransitNil

toMessagePack :: Maybe Message -> MP.Object
toMessagePack = fromTransit . toTransit

dumpMessage :: Maybe Message -> BS.ByteString
dumpMessage = encode . toMessagePack

readMessage :: BL.ByteString -> Maybe Message
readMessage bs = case decodeLazy bs of
                  Left _ -> Nothing
                  Right obj -> fromTransit (toTransit (obj :: MP.Object))

{-
readMessage :: BL.ByteString -> Either String BL.ByteString
readMessage msg = do
  obj <- decodeLazy msg
  let transitIn = toTransit (obj :: MP.Object)
  return $ dumpMessage . fmap handleMessage . fromTransit $ transitIn
-}
