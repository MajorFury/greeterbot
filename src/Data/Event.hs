{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Event (
    Event(..)
  , _NewUser
  , _NewAdmin
  , _UserLeft
  , _Other
  , readEvent
  , dumpEvent
) where

import Prelude

import Control.Applicative ( (<$>), (<*>))
import Control.Lens

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Map ()
import qualified Data.MessagePack as MP
import Data.Serialize (decodeLazy, encode)
import Data.Transit
import Data.UUID

type UserId = UUID
type GroupId = UUID

data Event = NewUser [GroupId] UserId
           | NewAdmin GroupId UserId
           | UserLeft GroupId UserId
           | Other Transit
  deriving (Eq, Show)
makePrisms ''Event

--transitLookup :: Text -> Maybe Transit
transitLookup :: (Control.Lens.Index m ~ Transit, Applicative f, At m) =>
                    Text -> (IxValue m -> f (IxValue m)) -> m -> f m
transitLookup k = at (TransitKeyword k) . _Just

makeNewUser :: [Transit] -> Maybe Event
makeNewUser [TransitMap m] = NewUser <$>
                              Just (m ^.. transitLookup "group-ids" . _TransitArray . each . _TransitUUID) <*>
                              m ^? transitLookup "id" . _TransitUUID
makeNewUser _ = Nothing

makeNewAdmin :: [Transit] -> Maybe Event
makeNewAdmin [TransitArray [TransitUUID groupId, TransitUUID adminId]] = Just $ NewAdmin groupId adminId
makeNewAdmin _ = Nothing

parseEvent :: Text -> [Transit] -> Maybe Event
parseEvent k info = case k of
                      "braid.client/new-user" -> makeNewUser info
                      "braid.client/new-admin" -> makeNewAdmin info
                      _ -> Nothing

instance Transitable Event where
  fromTransit eventInfo@(TransitArray ((TransitKeyword k):rest)) = case parseEvent k rest of
                                                                     Just e -> e
                                                                     Nothing -> Other eventInfo
  fromTransit eventInfo = Other eventInfo

  -- TODO: implement toTransit (although we don't need to send events)
  toTransit (Other t) = t
  toTransit _ = TransitNil

{-
braid.client/thread
braid.client/init-data
socket/connected
braid.client/create-tag
braid.client/joined-group
braid.client/update-users
braid.client/invitation-received
braid.client/name-change
braid.client/user-new-avatar
braid.client/left-group
braid.client/user-connected
braid.client/user-disconnected
braid.client/new-user
braid.client/user-left
braid.client/new-admin
braid.client/tag-descrption-change
braid.client/retract-tag
braid.client/new-intro
braid.client/group-new-avatar
braid.client/publicity-changed
braid.client/new-bot
braid.client/retract-bot
braid.client/edit-bot
braid.client/notify-message
braid.client/hide-thread
braid.client/show-thread
-}

toMessagePack :: Event -> MP.Object
toMessagePack = fromTransit . toTransit

fromMessagePack :: MP.Object -> Event
fromMessagePack = fromTransit . toTransit

dumpEvent :: Event -> BS.ByteString
dumpEvent = encode . toMessagePack

readEvent :: BL.ByteString -> Maybe Event
readEvent bs = case decodeLazy bs of
                Left _ -> Nothing
                Right obj -> Just $ fromMessagePack obj
