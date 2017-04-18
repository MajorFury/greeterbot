{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import qualified Data.Map as M
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import Data.Transit
import Data.Message
import Data.Event

main :: IO ()
main = hspec $ do
  describe "Message from Transit" $ do
    it "can parse a message from a transit map" $ do
      shouldBe (fromTransit (TransitMap $ M.fromList [ (TransitKeyword "id", TransitUUID UUID.nil)
                                                     , (TransitKeyword "group-id", TransitUUID UUID.nil)
                                                     , (TransitKeyword "thread-id", TransitUUID UUID.nil)
                                                     , (TransitKeyword "user-id", TransitUUID UUID.nil)
                                                     , (TransitKeyword "content", TransitString "foobar")
                                                     , (TransitKeyword "mentioned-tag-ids", TransitArray [TransitUUID UUID.nil])
                                                     , (TransitKeyword "mentioned-user-ids", TransitArray [])]))
               (Just Message { _messageId = UUID.nil
                             , _messageGroupId = UUID.nil
                             , _messageUserId  = UUID.nil
                             , _messageThreadId  = UUID.nil
                             , _messageContent  = "foobar"
                             , _messageMentionedTags = [UUID.nil]
                             , _messageMentionedUsers  = [] })
  describe "Event from Transit" $ do
    it "can parse an event from a transit map" $ do
      userId <- UUIDv4.nextRandom
      groupId <- UUIDv4.nextRandom
      shouldBe (fromTransit (TransitArray [ TransitKeyword "braid.client/new-user"
                                          , TransitArray
                                            [ TransitMap (M.fromList [ (TransitKeyword "id", TransitUUID userId)
                                                                     , (TransitKeyword "group-ids", TransitList [TransitUUID groupId])
                                                                   ])
                                            , TransitUUID groupId ]
                                          ]))
               (NewUser groupId userId)
