import Attribute
import EventLoop (run)
import Html (Html (..))
import qualified Html

data Message = Add | UpdateField String

data Model = Model
  { _modelField :: String,
    _modelEntries :: [String]
  }

mkModel :: Model
mkModel =
  Model
    { _modelField = "",
      _modelEntries = []
    }

view :: Model -> Html Message
view model =
  Html.div
    []
    [ viewInput
        (_modelField model),
      Html.ul
        []
        (map (\entry -> Html.li [] [Text entry]) (_modelEntries model))
    ]

viewInput :: String -> Html Message
viewInput task =
  Html.header
    [className "header"]
    [ Html.h1 [] [Text "Todos"],
      Html.form
        [onSubmit Add]
        [ Html.input
            [ className "new-todo",
              attr "placeholder" "What needs to be done?",
              attr "autofocus" "true",
              attr "value" task,
              attr "name" "newTodo",
              onInput UpdateField
            ]
            []
        ]
    ]

update :: Model -> Message -> IO Model
update model msg = return $ case msg of
  Add ->
    model
      { _modelField = "",
        _modelEntries = _modelEntries model ++ [_modelField model]
      }
  UpdateField value -> model {_modelField = value}

main :: IO ()
main = run view mkModel update
