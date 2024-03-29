import Attribute
import EventLoop (run)
import Html (Html (..))
import qualified Html

data Message = Increment | Decrement

view :: Int -> Html Message
view count =
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
main = run view 0 update
