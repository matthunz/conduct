module Attribute where

data AttributeValue m = StringValue String | HandlerValue m

data Attribute m = Attribute String (AttributeValue m)

onClick :: m -> Attribute m
onClick f = Attribute "click" (HandlerValue f)