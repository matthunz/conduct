{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module EventLoop where

import Attribute
import Control.Concurrent
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Foreign
import Foreign.C
import Foreign.C.String
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

callback :: CString -> IO ()
callback event = do
  s <- peekCString event
  ( let jsonByteString = BLU.fromString s
        maybeMyData = decode jsonByteString :: Maybe CallbackEvent
     in print maybeMyData
    )

run app = do
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
  callbackW <- wrap callback
  start callbackW
