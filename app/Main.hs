module Main where

import Lib
import Schema.Database


main :: IO ()
main = createPgConn >>= runDB (createPost "Some author" 1) >> print "OK. Post created."
