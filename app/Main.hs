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

printUsers :: IO()
printUsers  = withConn "tools.db" $
              \conn -> do
                  resp <- query_ conn "SELECT * FROM users;" :: IO [User]
                  mapM_ print resp

printToolQuery :: Query -> IO()
printToolQuery q = withConn "tools.db" $
                       \conn -> do
                           resp <- query_ conn q :: IO [Tool]
                           mapM_ print resp

printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools;"
                           
printAvailable :: IO ()
printAvailable = printToolQuery $ mconcat [ "select * from tools "
                                          , "where id not in "
                                          , "(select tool_id from checkedout);"]
                           
printCheckedout :: IO ()
printCheckedout = printToolQuery $
                            mconcat [ "select * from tools "
                                    , "where id in "
                                    , "(select tool_id from checkedout);"]

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
    resp <- query conn
            "SELECT * FROM tools WHERE id = (?) "
            (Only toolId) :: IO [Tool]
    return $ firstOrNothing resp 

firstOrNothing :: [a] -> Maybe abs 
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x 

updateTool :: Tool -> Day -> Tool
updateTool tool date = tool
                       { lastReturned = date
                       , timesBorrowed = 1 + timesBorrowed tool
                       }

updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = print "id not found"
                       updateOrWarn (Just tool) = withConn "tools.db" $
                        \conn -> do
                        let q = mconcat ["UPDATE TOOLS SET "
                        ,"lastReturned = ?,"
                        ," timesBorrowed = ? "
                        ,"WHERE ID = ?;"]
                        execute conn q (lastReturned tool
                        , timesBorrowed tool
                        , toolId tool)
                        print "tool updated"


main :: IO ()
main = print "db-lesson"
