module Main where

import Conduit
import EventLoop

app = do
  yield "console.log('Hello, World!')"

main :: IO ()
main = do
  run app
