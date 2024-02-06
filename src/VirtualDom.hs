{-# LANGUAGE LambdaCase #-}

module VirtualDom where

import Attribute
import Control.Monad (msum)
import Data.List (find)
import Debug.Trace (trace)
import Html

data Mutation m
  = CreateElement String Int Int
  | CreateTextNode String Int Int
  | Remove Int
  | SetAttribute Int (Attribute m)
  | SetText Int String

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
    Remove id -> "{ kind: 'remove', id: " ++ show id ++ " }"
    SetAttribute id (Attribute name value) ->
      "{ kind: 'set-attribute', id: "
        ++ show id
        ++ ", name: "
        ++ show name
        ++ ( case value of
               StringValue s -> ", value: " ++ show s
               HandlerValue _ -> ""
           )
        ++ " }"
    SetText id content ->
      "{ kind: 'set-text', id: "
        ++ show id
        ++ ", content: "
        ++ show content
        ++ " }"

data Node m = ElementNode Int [Attribute m] [Node m] | TextNode Int String

handle :: Int -> String -> Event -> Node m -> Maybe m
handle id name event node = case node of
  ElementNode nodeId attrs children ->
    if id == nodeId
      then
        find (\(Attribute attrName _) -> attrName == name) attrs
          >>= ( \(Attribute _ kind) -> case kind of
                  HandlerValue f -> Just (f event)
                  StringValue _ -> Nothing
              )
      else msum $ map (handle id name event) children
  TextNode _ _ -> Nothing

data VirtualDom = VirtualDom Int Int

mkVirtualDom :: VirtualDom
mkVirtualDom = VirtualDom 1 0

buildElement ::
  VirtualDom ->
  String ->
  [Attribute m] ->
  [Html m] ->
  (VirtualDom, Node m, [Mutation m])
buildElement (VirtualDom nextId parentId) tag attrs children =
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
   in ( vdom,
        ElementNode nextId attrs childNodes,
        CreateElement tag nextId parentId : attrMutations ++ mutations
      )

buildText :: VirtualDom -> String -> (VirtualDom, Node m, [Mutation m])
buildText (VirtualDom nextId parentId) content =
  ( VirtualDom (nextId + 1) nextId,
    TextNode nextId content,
    [CreateTextNode content nextId parentId]
  )

build :: VirtualDom -> Html m -> (VirtualDom, Node m, [Mutation m])
build vdom html = case html of
  Element tag attrs children -> buildElement vdom tag attrs children
  Text content -> buildText vdom content

rebuildElement ::
  VirtualDom ->
  String ->
  [Attribute m] ->
  [Html m] ->
  Node m ->
  (VirtualDom, Node m, [Mutation m])
rebuildElement (VirtualDom nextId parentId) tag attrs children node =
  case node of
    ElementNode nodeId nodeAttrs nodeChildren ->
      let (vdom, childNodes, mutations) =
            foldr
              ( \(child, childNode)
                 ( VirtualDom childNextId childParentId,
                   childNodes2,
                   childMutations
                   ) ->
                    let ( VirtualDom innerChildNextId _,
                          innerNode,
                          innerMutations
                          ) =
                            rebuild
                              (VirtualDom childNextId childParentId)
                              child
                              childNode
                     in ( VirtualDom innerChildNextId childParentId,
                          innerNode : childNodes2,
                          innerMutations ++ childMutations
                        )
              )
              (VirtualDom (nextId + 1) nextId, [], [])
              (zip children nodeChildren)
          (vdom3, childNodes3, mutations3) =
            let childrenLength = length children
                childNodesLength = length childNodes
             in if childrenLength > childNodesLength
                  then
                    foldr
                      ( \child (vdom2, childNodes2, mutations2) ->
                          let (vdom3, node, ms) = build vdom2 child
                           in (vdom3, node : childNodes2, ms)
                      )
                      (vdom, [], [])
                      (take childNodesLength $ reverse children)
                  else
                    if childrenLength < childNodesLength
                      then
                        ( vdom,
                          [],
                          map
                            ( \case
                                ElementNode id _ _ -> Remove id
                                TextNode id _ -> Remove id
                            )
                            (take childrenLength $ reverse childNodes)
                        )
                      else (vdom, [], [])
          attrMutations = map (SetAttribute nodeId) attrs
       in ( vdom3,
            ElementNode nodeId nodeAttrs (childNodes ++ childNodes3),
            mutations ++ attrMutations ++ mutations3
          )

rebuildText ::
  VirtualDom ->
  String ->
  Node m ->
  (VirtualDom, Node m, [Mutation m])
rebuildText (VirtualDom nextId parentId) content node =
  case node of
    ElementNode id _ _ ->
      ( VirtualDom (nextId + 1) parentId,
        TextNode nextId content,
        [Remove id, CreateTextNode content nextId parentId]
      )
    TextNode id nodeContent ->
      if content /= nodeContent
        then
          ( VirtualDom nextId parentId,
            TextNode id content,
            [SetText id content]
          )
        else (VirtualDom (nextId + 1) parentId, TextNode id content, [])

rebuild ::
  VirtualDom ->
  Html m ->
  Node m ->
  (VirtualDom, Node m, [Mutation m])
rebuild vdom html = case html of
  Element tag attrs children -> rebuildElement vdom tag attrs children
  Text content -> rebuildText vdom content
