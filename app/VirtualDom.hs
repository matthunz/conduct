module VirtualDom where

import Attribute
import Control.Monad (msum)
import Data.List (find)
import Html

data Mutation m
  = CreateElement String Int Int
  | CreateTextNode String Int Int
  | SetAttribute Int (Attribute m)

toJson :: Mutation m -> String
toJson mutation =
  case mutation of
    CreateElement tag id parentId ->
      "{ kind: 'create-element', id: "
        ++ show id
        ++ ", parentId: "
        ++ show parentId
        ++ ", tag: "
        ++ show tag
        ++ " }"
    CreateTextNode content id parentId ->
      "{ kind: 'create-text-node', id: "
        ++ show id
        ++ ", parentId: "
        ++ show parentId
        ++ ", content: "
        ++ show content
        ++ " }"
    SetAttribute id (Attribute name value) ->
      "{ kind: 'set-attribute', id: "
        ++ show id
        ++ ", name: "
        ++ show name
        ++ ( case value of
               StringValue s -> ", value: " ++ s
               HandlerValue _ -> ""
           )
        ++ " }"

data Node m = ElementNode Int [Attribute m] [Node m] | TextNode Int String

handle :: Int -> String -> Node m -> Maybe m
handle id name node = case node of
  ElementNode nodeId attrs children ->
    if id == nodeId
      then
        find (\(Attribute attrName _) -> attrName == name) attrs
          >>= ( \(Attribute _ kind) -> case kind of
                  HandlerValue f -> Just f
                  StringValue _ -> Nothing
              )
      else msum $ map (handle id name) children
  TextNode _ _ -> Nothing

data VirtualDom = VirtualDom Int Int

mkVirtualDom :: VirtualDom
mkVirtualDom = VirtualDom 1 0

build :: VirtualDom -> Html m -> (VirtualDom, Node m, [Mutation m])
build (VirtualDom nextId parentId) html = case html of
  Element tag attrs children ->
    let (vdom, childNodes, mutations) =
          foldr
            ( \child
               ( VirtualDom childNextId childParentId,
                 childNodes2,
                 childMutations
                 ) ->
                  let ( VirtualDom innerChildNextId _,
                        innerNode,
                        innerMutations
                        ) =
                          build (VirtualDom childNextId childParentId) child
                   in ( VirtualDom innerChildNextId childParentId,
                        innerNode : childNodes2,
                        innerMutations ++ childMutations
                      )
            )
            (VirtualDom (nextId + 1) nextId, [], [])
            children
        attrMutations = map (SetAttribute nextId) attrs
     in (vdom, ElementNode nextId attrs childNodes, CreateElement tag nextId parentId : attrMutations ++ mutations)
  Text content ->
    ( VirtualDom (nextId + 1) nextId,
      TextNode nextId content,
      [CreateTextNode content nextId parentId]
    )
