# Conduct
A cross-platform UI framework for Haskell.

This package provides an [Elm](https://elm-lang.org)-style
user interface library for [Tauri](https://tauri.app).

```hs
import Attribute
import EventLoop ( run )
import Html (Html (..))
import qualified Html

data Message = Increment | Decrement

app :: Int -> Html Message
app count =
  Html.div
    []
    [ Text $ "High five count: " ++ show count,
      Html.button [onClick Increment] [Text "Up high!"],
      Html.button [onClick Decrement] [Text "Down low!"]
    ]

update :: Int -> Message -> IO Int
update count msg = return $ case msg of
  Increment -> count + 1
  Decrement -> count - 1

main :: IO ()
main = run app 0 update
```