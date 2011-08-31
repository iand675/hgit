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
ptrMaybe :: (Ptr a -> b) -> Ptr a -> Maybe b
ptrMaybe w ptr = if ptr == nullPtr then Nothing else Just $ w ptr

ptrFunc f m = alloca $ \ptrptr -> do 
  result <- f ptrptr
  ptr <- peek ptrptr
  result `errorOr` m ptr

ptrFunc' f m = fmap m $ alloca $ \ptr -> do
  result <- f ptr
  result `errorOr` return ()
  

stateMod :: Integral a1 => (Ptr a -> IO a1) -> ForeignPtr a -> IO ()
stateMod f p = withForeignPtr p $ \p' -> do
  result <- f p'
  result `errorOr` return ()
  
stateMod' = flip withForeignPtr
