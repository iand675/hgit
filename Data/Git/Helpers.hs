module Data.Git.Helpers where
import Data.Git.Errors
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.Storable

boolHelper f val = withForeignPtr val f >>= return . (/= 0)

maybeCStr Nothing  = return nullPtr
maybeCStr (Just s) = newCString s

maybePtr Nothing  = return nullPtr
maybePtr (Just s) = malloc

ptrFunc f m = alloca $ \ptrptr -> do 
  result <- f ptrptr
  ptr <- peek ptrptr
  free ptrptr
  result `errorOr` m ptr