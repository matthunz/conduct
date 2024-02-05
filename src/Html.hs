module Html
  ( Html (..),
    button,
    Html.div,
    form,
    header,
    h1,
    input,
    li,
    ul,
  )
where

import Attribute

data Html m = Element String [Attribute m] [Html m] | Text String

button :: [Attribute m] -> [Html m] -> Html m
button = Element "button"

div :: [Attribute m] -> [Html m] -> Html m
div = Element "div"

form :: [Attribute m] -> [Html m] -> Html m
form = Element "form"

header :: [Attribute m] -> [Html m] -> Html m
header = Element "header"

h1 :: [Attribute m] -> [Html m] -> Html m
h1 = Element "h1"

input :: [Attribute m] -> [Html m] -> Html m
input = Element "input"

li :: [Attribute m] -> [Html m] -> Html m
li = Element "li"

ul :: [Attribute m] -> [Html m] -> Html m
ul = Element "ul"
