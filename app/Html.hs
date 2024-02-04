module Html (Html(..), Html.div, button) where

import Attribute

data Html m = Element String [Attribute m] [Html m] | Text String

div = Element "div"

button = Element "button"

