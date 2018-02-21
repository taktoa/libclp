--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}

--------------------------------------------------------------------------------

module CLP.Internal
  ( module CLP.Internal -- FIXME: specific export list
  ) where

--------------------------------------------------------------------------------

import           Control.Concurrent         (threadDelay)
import           Control.Monad              (void)
import           Control.Monad.State.Strict

import           Data.Int
import           Data.Word

import           Foreign.C
import           Foreign.C.Error
import           Foreign.C.String
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable

import           Control.DeepSeq            (NFData, force)
import           Data.Data                  (Data)
import           GHC.Generics               (Generic)

--------------------------------------------------------------------------------

#include "../../../include/clp.h"

--------------------------------------------------------------------------------

foreign import ccall "../../../include/clp.h Clp_Version"
  clp_version
  :: IO CString

foreign import ccall "../../../include/clp.h Clp_VersionMajor"
  clp_versionMajor
  :: IO Int

foreign import ccall "../../../include/clp.h Clp_VersionMinor"
  clp_versionMinor
  :: IO Int

foreign import ccall "../../../include/clp.h Clp_VersionRelease"
  clp_versionRelease
  :: IO Int

--------------------------------------------------------------------------------

{# pointer *Clp_Simplex as Clp_Simplex newtype #}

{# pointer *Clp_Solve as Clp_Solve newtype #}

--------------------------------------------------------------------------------

foreign import ccall "../../../include/clp.h Clp_newModel"
  c_Clp_newModel
  :: IO Clp_Simplex

foreign import ccall "../../../include/clp.h Clp_deleteModel"
  c_Clp_deleteModel
  :: Clp_Simplex
  -> IO ()

foreign import ccall "../../../include/clp.h ClpSolve_new"
  c_ClpSolve_new
  :: IO Clp_Solve

foreign import ccall "../../../include/clp.h ClpSolve_delete"
  c_ClpSolve_delete
  :: Clp_Solve
  -> IO ()

--------------------------------------------------------------------------------

-- foreign import ccall "../../../include/clp.h Clp_loadProblem"
--   c_Clp_loadProblem
--   :: Clp_Simplex -- model
--   -> Int -- numcols
--   -> Int -- numrows
--   -> 

--------------------------------------------------------------------------------
