{-# LANGUAGE ForeignFunctionInterface #-}

module Conduct where

import Conduit
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad ((>=>))
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Foreign
import Foreign.C
import Foreign.C.String
import GHC.Conc (TVar, newTVar)
import GHC.Generics

foreign import ccall "wrapper"
  wrap :: (CString -> IO ()) -> IO (FunPtr (CString -> IO ()))

foreign import ccall "c_start" start :: FunPtr (CString -> IO ()) -> IO ()

foreign import ccall unsafe "c_eval" evalJs :: CString -> IO ()

run :: ConduitT String String IO () -> IO ()
run c = do
  chan <- newTChanIO

  let callback s = do
        atomically $ writeTChan chan s
      x = yieldM (atomically $ readTChan chan)

  forkIO $ runConduit $ x .| c .| mapM_C (`withCString` evalJs)
  callbackW <- wrap (peekCString >=> callback)
  start callbackW
