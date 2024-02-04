module Html (Html (..), Html.div, button) where

import Attribute

data Html m = Element String [Attribute m] [Html m] | Text String

div :: [Attribute m] -> [Html m] -> Html m
div = Element "div"

button :: [Attribute m] -> [Html m] -> Html m
button = Element "button"
