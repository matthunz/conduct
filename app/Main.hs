{-# LANGUAGE ForeignFunctionInterface #-}

import Control.Concurrent
import Control.Monad
import Foreign.C.String
import System.IO.Unsafe

foreign import ccall  "c_start" start :: IO ()

foreign import ccall unsafe "c_eval" evalJs :: CString -> IO ()

main :: IO ()
main = do
    _ <-
        forkIO
            ( do
                -- TODO uhh...
                _ <- threadDelay 1000000
                withCString "console.log('test')" evalJs
            )
    start
   
