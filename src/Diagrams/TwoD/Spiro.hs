{-# LANGUAGE GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , FlexibleContexts
           , FlexibleInstances
           , StandaloneDeriving
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Spiro
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Generic functionality for constructing cubic splines
--
-----------------------------------------------------------------------------
module Diagrams.TwoD.Spiro
       ( spiro
       , SpiroPoint(..)
       , SpiroConstraint(..)
       ) where

import System.IO.Unsafe(unsafePerformIO)

import Graphics.Spiro.Raw

import Diagrams.Prelude
import Diagrams.TwoD.Spiro.PathContext (PathContext,toDiagram,zeroPathContext)
import qualified Diagrams.TwoD.Spiro.PathContext as P

import Data.Monoid
import Data.NumInstances

import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad
import Control.Monad.State

import Control.Newtype

newtype PathPostscriptT s m a = PathPostscriptT { runPathPostscriptT :: StateT (PathContext s) m a }
    deriving (Functor,Applicative,Monad,MonadIO,MonadTrans)

deriving instance Monad m => MonadState (PathContext s) (PathPostscriptT s m)

instance Newtype (PathPostscriptT s m a) (StateT (PathContext s) m a) where
  pack   = PathPostscriptT
  unpack = runPathPostscriptT

instance MonadIO m => PostscriptLike (PathPostscriptT R2 m) where
  moveTo' x y b             = modify $ P.moveTo' (x,y) b
  moveTo  x y               = modify $ P.moveTo  (x,y)
  lineTo  x y               = modify $ P.lineTo  (x,y)
  quadTo  x y x' y'         = modify $ P.quadTo  (x,y) (x',y')
  curveTo x y x' y' x'' y'' = modify $ P.curveTo (x,y) (x',y') (x'',y'')
  markKnot _ = return ()

-- | Computes a diagram from the given spiro control points.
spiro :: Renderable (Path R2) b 
      => Bool         -- ^ 'True' for a closed path.
      -> [SpiroPoint] -- ^ Set of control points with their constrints.
      -> Diagram b R2
spiro closed ps = unsafePerformIO $ do
  let p = runPathPostscriptT $ spiroToBezier closed ps
  execStateT p zeroPathContext >>= return . toDiagram
