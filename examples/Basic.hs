module Main where

import Conduct
import Conduit

app = do
  yield "console.log('Hello, World!')"

main :: IO ()
main = do
  run app
