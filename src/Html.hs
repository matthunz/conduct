module Html
  ( Html (..),
    button,
    Html.div,
    header,
    h1,
    input,
  )
where

import Attribute

data Html m = Element String [Attribute m] [Html m] | Text String

button :: [Attribute m] -> [Html m] -> Html m
button = Element "button"

div :: [Attribute m] -> [Html m] -> Html m
div = Element "div"

header :: [Attribute m] -> [Html m] -> Html m
header = Element "header"

h1 :: [Attribute m] -> [Html m] -> Html m
h1 = Element "h1"

input :: [Attribute m] -> [Html m] -> Html m
input = Element "input"
