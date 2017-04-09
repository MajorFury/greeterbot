{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import qualified Data.Map as M
import qualified Data.UUID as UUID
import Data.Transit
import Data.Message

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
               (Just $ Message { _messageId = UUID.nil
                , _messageGroupId = UUID.nil
                , _messageUserId  = UUID.nil
                , _messageThreadId  = UUID.nil
                , _messageContent  = "foobar"
                , _messageMentionedTags = [UUID.nil]
                , _messageMentionedUsers  = [] })
