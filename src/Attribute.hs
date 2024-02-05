{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Attribute where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

data Event
  = Click
  | Input {inputValue :: String}
  | Submit
  deriving (Show, Generic)

instance FromJSON Event

data AttributeValue m = StringValue String | HandlerValue (Event -> m)

data Attribute m = Attribute String (AttributeValue m)

onClick :: m -> Attribute m
onClick f = Attribute "click" (HandlerValue $ const f)

onSubmit :: m -> Attribute m
onSubmit f = Attribute "submit" (HandlerValue $ const f)

onInput :: (String -> m) -> Attribute m
onInput f =
  Attribute
    "input"
    ( HandlerValue
        ( \case
            Input value -> f value
            _ -> error ""
        )
    )

attr :: String -> String -> Attribute m
attr name value = Attribute name (StringValue value)

className :: String -> Attribute m
className = attr "class"
