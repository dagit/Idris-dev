-- | Platform-specific dynamic linking support. Add new platforms to this file
-- through conditional compilation.
{-# LANGUAGE ExistentialQuantification, CPP #-}
module Util.DynamicLinker
( DynamicLib(..)
, hostDynamicLibExt
, tryLoadLib
, ForeignFun(..)
, tryLoadFn
, Handle
) where

import Foreign.Ptr (nullPtr, FunPtr, nullFunPtr
                   ,castPtrToFunPtr)
#if (LINUX || MACOSX)
import Foreign.LibFFI
import System.Posix.DynamicLinker
#elif WINOWS
import System.Win32.DLL
#endif

hostDynamicLibExt :: String
#ifdef LINUX
hostDynamicLibExt = "so"
#elif MACOSX
hostDynamicLibExt = "dylib"
#elif WINDOWS
hostDynamicLibExt = "dll"
#endif

-- The handle type is intentionally opaque outside
-- this module.
#if (LINUX || MACOSX)
data Handle = Handle DL
#elif WINDOWS
data Handle = Handle HINSTANCE
#endif

data DynamicLib = Lib { lib_name   :: String
                      , lib_handle :: Handle
                      }

dlopen_platform :: String -> IO (Maybe Handle)
#if (LINUX || MACOSX)
dlopen_platform s = do
  handle <- dlopen s [RTLD_NOW])
  return $ case undl handle == nullPtr of
           False -> Nothing
           True  -> Just (Handle handle)
#elif WINDOWS
dlopen_platform s = do
  handle <- loadLibrary s
  return $ case handle == nullPtr of
           False -> Nothing
           True  -> Just (Handle handle)
#endif

tryLoadLib :: String -> IO (Maybe DynamicLib)
tryLoadLib lib = do handle <- dlopen_platform (lib ++ "." ++ hostDynamicLibExt)
                    return (Lib lib `fmap` handle)

data ForeignFun = forall a. Fun { fun_name   :: String
                                , fun_handle :: FunPtr a
                                }

dlsym_platform :: Handle -> String -> IO (Maybe (FunPtr a))
#if (LINUX || MACOSX)
dlsym_platform h fn = do
  cFn <- dlsym h fn
  return $ case cFn == nullFunPtr of
           False -> Nothing
           True  -> Just cFn
#elif WINOWS
dlsym_platform h fn = do
  cFn <- getProcAddress h fn
  return $ case cFn == nullPtr of
           False -> Nothing
           True  -> Just (castPtrToFunPtr cFn)
#endif

tryLoadFn :: String -> DynamicLib -> IO (Maybe ForeignFun)
tryLoadFn fn (Lib _ h) = do
  cFn <- dlsym_platform h fn
  return (Fun fn `fmap` cFn)
