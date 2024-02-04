{-# LANGUAGE ForeignFunctionInterface #-}

import Control.Concurrent
import Control.Monad
import Foreign.C.String
import System.IO.Unsafe

foreign import ccall "c_start" start :: IO ()

foreign import ccall unsafe "c_eval" evalJs :: CString -> IO ()

data Mutation
    = CreateElement String Int Int
    | CreateTextNode String Int Int

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

data Html = Element String [Html] | Text String

data VirtualDom = VirtualDom Int Int

mkVirtualDom :: VirtualDom
mkVirtualDom = VirtualDom 1 0

build :: VirtualDom -> Html -> (VirtualDom, [Mutation])
build (VirtualDom nextId parentId) html = case html of
    Element tag children ->
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
         in (vdom, CreateElement tag nextId parentId : mutations)
    Text content ->
        ( VirtualDom (nextId + 1) nextId
        , [CreateTextNode content nextId parentId]
        )

div_ = Element "div"

button = Element "button"

app :: Html
app =
    div_
        [ Text "Hello World!"
        , button [Text "Up high!"]
        , button [Text "Down low!"]
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
