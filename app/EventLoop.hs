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
    name :: String
  }
  deriving (Show, Generic)

instance FromJSON CallbackEvent

callback ::
  (s -> Html m) ->
  TVar (s, Maybe (Node m)) ->
  (s -> m -> IO s) ->
  CString ->
  IO ()
callback app var update event = do
  s <- peekCString event
  ( let jsonByteString = BLU.fromString s
        maybeMyData = decode jsonByteString :: Maybe CallbackEvent
     in case maybeMyData of
          Just (CallbackEvent id name) -> do
            (state, nodeCell) <- readTVarIO var
            _ <-
              ( case nodeCell of
                  Just node ->
                    case handle id name node of
                      Just msg -> do
                        newState <- update state msg
                        return ()
                      Nothing -> return ()
                  Nothing ->
                    return ()
                )
            return ()
          Nothing -> return ()
    )

run app state update = do
  stateVar <- newTVarIO (state, Nothing)
  _ <-
    forkIO
      ( let (_, node, mutations) = build mkVirtualDom (app state)
         in do
              atomically $ writeTVar stateVar (state, Just node)
              withCString
                ( "window.conduct.update(["
                    ++ concatMap (\m -> toJson m ++ ", ") mutations
                    ++ "])"
                )
                evalJs
      )
  callbackW <- wrap (callback app stateVar update)
  start callbackW
