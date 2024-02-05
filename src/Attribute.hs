module Attribute where

data AttributeValue m = StringValue String | HandlerValue m

data Attribute m = Attribute String (AttributeValue m)

onClick :: m -> Attribute m
onClick f = Attribute "click" (HandlerValue f)

onInput :: m -> Attribute m
onInput f = Attribute "input" (HandlerValue f)

attr :: String -> String -> Attribute m
attr name value = Attribute name (StringValue value)

className :: String -> Attribute m
className = attr "class"
