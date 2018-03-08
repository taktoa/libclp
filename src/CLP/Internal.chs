--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}

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

newtype Simplex
  = Simplex (ForeignPtr ())

foreign import ccall "../../../include/clp.h Clp_newModel"
  c_Clp_newModel
  :: IO (Ptr ())

foreign import ccall "../../../include/clp.h &Clp_deleteModel"
  c_Clp_deleteModel :: FinalizerPtr ()

createSimplex :: IO Simplex
createSimplex = do
  ptr <- c_Clp_newModel
  fptr <- newForeignPtr c_Clp_deleteModel ptr
  pure (Simplex fptr)

--------------------------------------------------------------------------------

data Clp_Solve

newtype Solve
  = Solve (ForeignPtr Clp_Solve)

foreign import ccall "../../../include/clp.h ClpSolve_new"
  c_ClpSolve_new
  :: IO (Ptr Clp_Solve)

foreign import ccall "../../../include/clp.h &ClpSolve_delete"
  c_ClpSolve_delete :: FinalizerPtr Clp_Solve

createSolve :: IO Solve
createSolve = do
  ptr <- c_ClpSolve_new
  fptr <- newForeignPtr c_ClpSolve_delete ptr
  pure (Solve fptr)

--------------------------------------------------------------------------------

data OptimizationDir
  = Minimize
  | Maximize
  | Ignore
  deriving ()

instance Storable OptimizationDir where
  sizeOf    _ = sizeOf    (undefined :: Double)
  alignment _ = alignment (undefined :: Double)

  peek p = do
    result <- id @Double <$> peek (castPtr p)
    case result of
      (1.0)  -> pure Minimize
      (-1.0) -> pure Maximize
      0.0    -> pure Ignore
      _      -> fail "Something went wrong when marshalling an OptimizationDir"

  poke p = \case
    Minimize -> poke (castPtr p) (1.0  :: Double)
    Maximize -> poke (castPtr p) (-1.0 :: Double)
    Ignore   -> poke (castPtr p) (0.0  :: Double)

--------------------------------------------------------------------------------


-- foreign import ccall "../../../include/clp.h ClpSolve_new"
--   c_ClpSolve_new
--   :: IO (Ptr ())

--------------------------------------------------------------------------------
