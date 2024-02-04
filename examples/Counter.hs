import Attribute
import EventLoop
import Html (Html (..))
import qualified Html

data Message = Increment | Decrement

app :: Int -> Html Message
app count =
  Html.div
    []
    [ Text $ "High five count" ++ show count,
      Html.button [onClick Increment] [Text "Up high!"],
      Html.button [onClick Decrement] [Text "Down low!"]
    ]

main :: IO ()
main = run $ app 0
