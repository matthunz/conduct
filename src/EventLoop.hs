{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module EventLoop where

import Attribute
import Control.Concurrent
import Control.Concurrent.STM
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Foreign
import Foreign.C
import Foreign.C.String
import GHC.Conc (TVar, newTVar)
import GHC.Generics
import Html (Html (..))
import qualified Html
import VirtualDom

foreign import ccall "wrapper"
  wrap :: (CString -> IO ()) -> IO (FunPtr (CString -> IO ()))

foreign import ccall "c_start" start :: FunPtr (CString -> IO ()) -> IO ()

foreign import ccall unsafe "c_eval" evalJs :: CString -> IO ()

data CallbackEvent = CallbackEvent
  { id :: Int,
    name :: String,
    event :: Event
  }
  deriving (Show, Generic)

instance FromJSON CallbackEvent

callback ::
  (s -> Html m) ->
  TVar (s, Maybe (VirtualDom, Node m)) ->
  (s -> m -> IO s) ->
  CString ->
  IO ()
callback app var update event = do
  s <- peekCString event
  ( let jsonByteString = BLU.fromString s
        maybeMyData = decode jsonByteString :: Maybe CallbackEvent
     in case maybeMyData of
          Just (CallbackEvent id name event) -> do
            (state, nodeCell) <- readTVarIO var
            _ <-
              ( case nodeCell of
                  Just (VirtualDom nextId _, node) ->
                    case handle id name event node of
                      Just msg -> do
                        newState <- update state msg
                        ( let (newVdom, newNode, mutations) = rebuild (VirtualDom nextId 0) (app newState) node
                           in do
                                atomically $ writeTVar var (newState, Just (newVdom, newNode))
                                withCString
                                  ( "window.conduct.update(["
                                      ++ concatMap (\m -> toJson m ++ ", ") mutations
                                      ++ "])"
                                  )
                                  evalJs
                          )
                        return ()
                      Nothing -> return ()
                  Nothing ->
                    return ()
                )
            return ()
          Nothing -> return ()
    )

run :: (s -> Html m) -> s -> (s -> m -> IO s) -> IO ()
run app state update = do
  stateVar <- newTVarIO (state, Nothing)
  _ <-
    forkIO
      ( let (vdom, node, mutations) = build mkVirtualDom (app state)
         in do
              atomically $ writeTVar stateVar (state, Just (vdom, node))
              withCString
                ( "window.conduct.update(["
                    ++ concatMap (\m -> toJson m ++ ", ") mutations
                    ++ "])"
                )
                evalJs
      )
  callbackW <- wrap (callback app stateVar update)
  start callbackW
