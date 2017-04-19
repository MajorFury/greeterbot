{-# LANGUAGE OverloadedStrings #-}
module Db (
    initDB
  , sawUser
  , didSeeUser
) where

import Prelude

import qualified Data.Text as T
import qualified Data.UUID as U
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow ()

data SeenUser = SeenUser Int T.Text deriving (Show)

instance FromRow SeenUser where
  fromRow = SeenUser <$> field <*> field

instance ToRow SeenUser where
  toRow (SeenUser id_ userIdStr) = toRow (id_, userIdStr)

dbName :: String
dbName = "seen_users.sqlite"

initDB :: IO ()
initDB = do
  conn <- open dbName
  execute_ conn "CREATE TABLE IF NOT EXISTS seen_users (id INTEGER PRIMARY KEY, userIdStr TEXT)"
  close conn

sawUser :: U.UUID -> IO ()
sawUser userId = do
  conn <- open dbName
  execute conn "INSERT INTO seen_users (userIdStr) VALUES (?)" (Only (U.toString userId))
  close conn

didSeeUser :: U.UUID -> IO Bool
didSeeUser userId = do
  conn <- open dbName
  [[didSee]] <- query conn "SELECT EXISTS (SELECT id FROM seen_users WHERE userIdStr = ?)" (Only (U.toString userId))
  close conn
  return didSee
