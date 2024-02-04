{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C.String (CString, newCString)

foreign import ccall unsafe "c_hello" hello :: CString -> IO ()

main :: IO ()
main = do
    str <- newCString "World"
    hello str
