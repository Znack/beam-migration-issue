module Main where

import Lib
import Schema.Database


main :: IO ()
main = do
    conn <- createPgConn 
    runDB (createPost "Some author" 1) conn
    runDB selectPosts conn
    print "OK. Post created."
