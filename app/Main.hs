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

data User 

main :: IO ()
main = print "db-lesson"
