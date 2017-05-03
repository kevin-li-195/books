module Main where

import Server.Types ( renewalApi )

import Servant.JS
import Servant.JS.JQuery
import System.Environment ( getArgs )

main :: IO ()
main = do
  [out] <- getArgs
  writeJSForAPI renewalApi jquery out
