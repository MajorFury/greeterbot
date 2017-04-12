{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Event (
    Event(..)
  , eventType
  , eventPayload
  , readEvent
  , dumpEvent
) where

import Prelude

import Control.Lens

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.MessagePack as MP
import Data.Serialize (decodeLazy, encode)
import Data.Transit

data Event = Event {
    _eventType :: Text
  , _eventPayload :: [Transit]
} deriving (Eq, Show)
makeLenses ''Event

instance Transitable (Maybe Event) where
  fromTransit (TransitArray ((TransitKeyword eType) : rest)) = Just $ Event eType rest
  fromTransit _ = Nothing

  toTransit (Just Event{..}) = TransitArray ((TransitKeyword _eventType):_eventPayload)
  toTransit Nothing = TransitNil

toMessagePack :: Maybe Event -> MP.Object
toMessagePack = fromTransit . toTransit

fromMessagePack :: MP.Object -> Maybe Event
fromMessagePack = fromTransit . toTransit

dumpEvent :: Maybe Event -> BS.ByteString
dumpEvent = encode . toMessagePack

readEvent :: BL.ByteString -> Maybe Event
readEvent bs = case decodeLazy bs of
                Left _ -> Nothing
                Right obj -> fromMessagePack obj
