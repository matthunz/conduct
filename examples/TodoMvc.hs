import Attribute
import EventLoop (run)
import Html (Html (..))
import qualified Html

data Message = UpdateField String

data Model = Model
  { _modelField :: String
  }

mkModel :: Model
mkModel =
  Model
    { _modelField = ""
    }

view :: Model -> Html Message
view model =
  Html.div
    []
    [ viewInput (_modelField model)
    ]

viewInput :: String -> Html Message
viewInput task =
  Html.header
    [className "header"]
    [ Html.h1 [] [Text "Todos"],
      Html.input
        [ className "new-todo",
          attr "placeholder" "What needs to be done?",
          attr "autofocus" "true",
          attr "value" task,
          attr "name" "newTodo",
          onInput (UpdateField "test")
        ]
        []
    ]

update :: Model -> Message -> IO Model
update model msg = return $ case msg of
  UpdateField value -> model {_modelField = value}

main :: IO ()
main = run view mkModel update
