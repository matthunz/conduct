module Main where

import Conduct
import Conduit

app = do
  yield "window.ipc.postMessage('Hello, World!');"
  x <- await
  liftIO $ print x

main :: IO ()
main = do
  run app
