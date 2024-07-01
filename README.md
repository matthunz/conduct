# Conduct

[![CI](https://github.com/matthunz/conduct/actions/workflows/ci.yml/badge.svg)](https://github.com/matthunz/conduct/actions/workflows/ci.yml)

[Examples](https://github.com/matthunz/conduct/tree/main/examples)

A cross-platform driver for [Tauri](https://tauri.app) in Haskell.

```hs
import Conduct
import Conduit

app = do
  yield "window.ipc.postMessage('Hello, World!');"
  x <- await
  liftIO $ print x

main :: IO ()
main = do
  run app
```
