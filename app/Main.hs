{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C.String (CString, newCString)
import Control.Monad

foreign import ccall unsafe "c_hello" hello :: CString -> IO ()

main :: IO ()
main =  forever $ do
    str <- newCString "World"
    hello str
