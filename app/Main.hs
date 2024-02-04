{-# LANGUAGE ForeignFunctionInterface #-}

import Control.Concurrent
import Foreign.C.String

import Attribute
import Html (Html (..))
import qualified Html

foreign import ccall "c_start" start :: IO ()

foreign import ccall unsafe "c_eval" evalJs :: CString -> IO ()

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

data VirtualDom = VirtualDom Int Int

mkVirtualDom :: VirtualDom
mkVirtualDom = VirtualDom 1 0

build :: VirtualDom -> Html m -> (VirtualDom, [Mutation m])
build (VirtualDom nextId parentId) html = case html of
    Element tag attrs children ->
        let (vdom, mutations) =
                foldr
                    ( \child (VirtualDom childNextId childParentId, childMutations) ->
                        let (VirtualDom innerChildNextId _, innerMutations) =
                                build (VirtualDom childNextId childParentId) child
                         in ( VirtualDom innerChildNextId childParentId
                            , innerMutations ++ childMutations
                            )
                    )
                    ( VirtualDom (nextId + 1) nextId
                    , []
                    )
                    children
            attrMutations = map (SetAttribute nextId) attrs
         in (vdom, CreateElement tag nextId parentId : attrMutations ++ mutations)
    Text content ->
        ( VirtualDom (nextId + 1) nextId
        , [CreateTextNode content nextId parentId]
        )

data Message = Increment | Decrement

app :: Html Message
app =
    Html.div
        []
        [ Text "Hello World!"
        , Html.button [Attribute.onClick Increment] [Text "Up high!"]
        , Html.button [Attribute.onClick Decrement] [Text "Down low!"]
        ]

main :: IO ()
main = do
    _ <-
        forkIO
            ( let (_, mutations) = build mkVirtualDom app
               in do
                    withCString
                        ( "window.conduct.update(["
                            ++ concatMap (\m -> toJson m ++ ", ") mutations
                            ++ "])"
                        )
                        evalJs
            )
    start
