{-# OPTIONS -#include "libtcc.h" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------------------------
-- |
-- Module    :  Language.TCC
-- Copyright :  (c) Vo Minh Thu 2009
-- License   :  BSD3
-- Maintainer:  Vo Minh Thu <noteed@gmail.com>
-- Stability :  experimental
-- Portability: Requires FFI
--
--------------------------------------------------------------------
--
-- Haskell bindings to the Tiny C Compiler library (libtcc).
--

module Language.TCC where

import Foreign.C        (CInt, CULong)
import Foreign.C.String (CString, withCString)
import Foreign.Ptr      (Ptr, FunPtr)
import Data.Word        (Word32)
import Foreign.Storable (peek, Storable, sizeOf)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Control.Exception     (bracket)

--------------------------------------------------------------------
--
-- Bindings against TCC 0.9.24
--

-- TCCState *
newtype TCCState = TCCState (Ptr ()) deriving Storable

-- TCCState *tcc_new(void);
foreign import ccall "libtcc.h tcc_new" c_new
  :: IO TCCState

-- void tcc_delete(TCCState *s);
foreign import ccall "libtcc.h tcc_delete" c_delete
  :: TCCState -> IO ()

-- void tcc_enable_debug(TCCState *s);
-- Appears in libtcc.h but not in libtcc.a ...
--foreign import ccall "libtcc.h tcc_enable_debug" c_enable_debug
--  :: TCCState -> IO ()

-- void tcc_set_error_func(TCCState *s, void *error_opaque,
--                         void (*error_func)(void *opaque, const char *msg));
foreign import ccall "libtcc.h tcc_set_error_func" c_set_error_func
  :: TCCState -> Ptr () -> FunPtr (Ptr () -> CString -> IO ()) -> IO ()

-- int tcc_set_warning(TCCState *s, const char *warning_name, int value);
foreign import ccall "libtcc.h tcc_set_warning" c_set_warning
  :: TCCState -> CString -> CInt -> IO CInt

-- int tcc_add_include_path(TCCState *s, const char *pathname);
foreign import ccall "libtcc.h tcc_add_include_path" c_add_include_path
  :: TCCState -> CString -> IO CInt

-- int tcc_add_sysinclude_path(TCCState *s, const char *pathname);
foreign import ccall "libtcc.h tcc_add_sysinclude_path" c_add_sysinclude_path
  :: TCCState -> CString -> IO CInt

-- void tcc_define_symbol(TCCState *s, const char *sym, const char *value);
foreign import ccall "libtcc.h tcc_define_symbol" c_define_symbol
  :: TCCState -> CString -> CString -> IO ()

-- void tcc_undefine_symbol(TCCState *s, const char *sym);
foreign import ccall "libtcc.h tcc_undefine_symbol" c_undefine_symbol
  :: TCCState -> CString -> IO ()

-- int tcc_add_file(TCCState *s, const char *filename);
foreign import ccall "libtcc.h tcc_add_file" c_add_file
  :: TCCState -> CString -> IO CInt

-- int tcc_compile_string(TCCState *s, const char *buf);
foreign import ccall "libtcc.h tcc_compile_string" c_compile_string
  :: TCCState -> CString -> IO CInt

-- #define TCC_OUTPUT_MEMORY   0
-- #define TCC_OUTPUT_EXE      1
-- #define TCC_OUTPUT_DLL      2
-- #define TCC_OUTPUT_OBJ      3
-- #define TCC_OUTPUT_PREPROCESS 4
type OutputType = Word32
tcc_OUTPUT_MEMORY     :: OutputType
tcc_OUTPUT_MEMORY     = 0
tcc_OUTPUT_EXE        :: OutputType
tcc_OUTPUT_EXE        = 1
tcc_OUTPUT_DLL        :: OutputType
tcc_OUTPUT_DLL        = 2
tcc_OUTPUT_OBJ        :: OutputType
tcc_OUTPUT_OBJ        = 3
tcc_OUTPUT_PREPROCESS :: OutputType
tcc_OUTPUT_PREPROCESS = 4

-- int tcc_set_output_type(TCCState *s, int output_type);
foreign import ccall "libtcc.h tcc_set_output_type" c_set_output_type
  :: TCCState -> OutputType -> IO CInt

-- #define TCC_OUTPUT_FORMAT_ELF    0
-- #define TCC_OUTPUT_FORMAT_BINARY 1
-- #define TCC_OUTPUT_FORMAT_COFF   2
type OutputFormat = Word32
tcc_OUTPUT_FORMAT_ELF    :: OutputFormat
tcc_OUTPUT_FORMAT_ELF    = 0
tcc_OUTPUT_FORMAT_BINARY :: OutputFormat
tcc_OUTPUT_FORMAT_BINARY = 1
tcc_OUTPUT_FORMAT_COFF   :: OutputFormat
tcc_OUTPUT_FORMAT_COFF   = 2

-- int tcc_add_library_path(TCCState *s, const char *pathname);
foreign import ccall "libtcc.h tcc_add_library_path" c_add_library_path
  :: TCCState -> CString -> IO CInt

-- int tcc_add_library(TCCState *s, const char *libraryname);
foreign import ccall "libtcc.h tcc_add_library" c_add_library
  :: TCCState -> CString -> IO CInt

-- int tcc_add_symbol(TCCState *s, const char *name, unsigned long val);
foreign import ccall "libtcc.h tcc_add_symbol" c_add_symbol
  :: TCCState -> CString -> CULong -> IO CInt

-- int tcc_output_file(TCCState *s, const char *filename);
foreign import ccall "libtcc.h tcc_output_file" c_output_file
  :: TCCState -> CString -> IO CInt

-- int tcc_run(TCCState *s, int argc, char **argv);
foreign import ccall "libtcc.h tcc_run" c_run
  :: TCCState -> CInt -> Ptr CString -> IO CInt

-- int tcc_relocate(TCCState *s);
foreign import ccall "libtcc.h tcc_relocate" c_relocate
  :: TCCState -> IO CInt

-- int tcc_get_symbol(TCCState *s, unsigned long *pval, const char *name);
foreign import ccall "libtcc.h tcc_get_symbol" c_get_symbol
  :: TCCState -> Ptr CULong -> CString -> IO CInt

--------------------------------------------------------------------
--
-- Convenience functions
--

-- Similar to alloca but return both the computation result and
-- the value of the temporarily allocated block of memory.
-- See the code of allocaBytes for a GHC specific way to
-- rewrite this (with newPinedByteArray).
withAlloca :: Storable a => (Ptr a -> IO b) -> IO (a,b)
withAlloca f = bracket (doMalloc undefined) free g
  where g m = do b <- f m
                 a <- peek m
                 return (a,b)
        doMalloc :: Storable a' => a' -> IO (Ptr a')
        doMalloc dummy = mallocBytes $ sizeOf dummy

getSymbol :: TCCState -> String -> IO (Maybe CULong)
getSymbol s name = do
  (addr,ret) <- withCString name (\str ->
    withAlloca (\addr -> c_get_symbol s addr str))
  if ret == 0 then return (Just addr)
              else return Nothing

foreign import ccall "convenience.h call" c_call
  :: CULong -> IO ()

foreign import ccall "convenience.h calli" c_calli
  :: CULong -> CInt -> IO ()

