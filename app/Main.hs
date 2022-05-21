module Main where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time

import Lib

data Tool = Tool
            {
                toolId :: Int,
                name :: String,
                description :: String,
                lastReturned :: Day,
                timesBorrowed :: Int
            }

data User = User
            {
                userId :: Int,
                userName :: String
            }

class FromRow a where
    fromRow :: RowParser a

instance Show User where
    show user = mconcat [show $ userId user , ".) ", userName user]

instance Show Tool where
    show tool = mconcat [show $ toolId tool, ".) ", name tool , "\n description : ", description tool, "\n last returned: ", show $ lastReturned tool, "\n times borrowed: " , show $ timesBorrowed tool, "\n"]

instance FromRow User where
    fromRow = User <$> field
                   <*> field

instance FromRow Tool where
    fromRow :: Tool <$> field
                    <*> field
                    <*> field
                    <*> field
                    <*> field

addUser :: String -> IO()
addUser userName = do
    conn <- "tools.db"
    execute conn "INSERT INTO users (username) VALUES (?)"
      (Only userName)
    print "user added"
    close conn

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
     conn <- open dbName
     action conn
     close conn

query :: (FromRow r, ToRow q) => Connection -> Query -> q -> IO [r]
query_ :: FromRow r => Connection -> Query -> IO [r]

main :: IO ()
main = print "db-lesson"
