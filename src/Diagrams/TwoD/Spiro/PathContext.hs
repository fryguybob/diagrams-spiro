{-# LANGUAGE GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , TypeSynonymInstances
           , FlexibleContexts
           , FlexibleInstances
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.TwoD.Spiro.PathContext
-- Copyright   :  (c) 2011 diagrams-lib team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Generic functionality for constructing cubic splines
--
-----------------------------------------------------------------------------
module Diagrams.TwoD.Spiro.PathContext
       ( PathContext
       , zeroPathContext
       , initialPathContext
       , toDiagram
       , moveTo
       , moveTo'
       , lineTo
       , quadTo
       , curveTo
       , newPath
       , appendSegment
       , paths
       ) where

import Diagrams.Prelude hiding (moveTo)
import Data.VectorSpace
import Data.Monoid
import Control.Applicative

-- | State needed for constructing a diagram from a 'PostscriptLike' context over
--   some space @v@.
data PathContext v = C 
     { start    :: v
     , current  :: v
     , closed   :: Bool
     , segments :: [Segment v]
     , paths    :: [Path v] -- ^ Paths in context.
     }
  deriving (Show)

-- | An initial 'PathContext' at the given location.
initialPathContext :: v -> PathContext v
initialPathContext v = C v v False [] []

-- | An initial 'PathContext' at zero.
zeroPathContext :: Num v => PathContext v
zeroPathContext = initialPathContext 0

-- | Takes the accumulated state and builds a diagram.
toDiagram :: Renderable (Path R2) b => PathContext R2 -> Diagram b R2
toDiagram = mconcat . map stroke . reverse . paths . newPath

modifyStart   b c@(C a _ _ _ _) = c { start   = b }
modifyCurrent b c@(C _ a _ _ _) = c { current = b }
modifyClosed  b c@(C _ _ a _ _) = c { closed  = b }

segmentEnd :: Segment v -> v
segmentEnd (Linear v)    = v
segmentEnd (Cubic _ _ v) = v

-- | Adds a segment (in absolute corrdiantes) to the current path.  
appendSegment :: Num v => Segment v -> PathContext v -> PathContext v
appendSegment s (C a b c ss pp) = C a (segmentEnd s) c (s':ss) pp
  where s' = subtract b <$> s

-- | Start a new path.
newPath :: PathContext v -> PathContext v
newPath c@(C _ b _ [] _ ) = c { closed = False, start = b }
newPath   (C a b c ss pp) = C b b False [] (p:pp)
  where p = Path [(P a, Trail (reverse ss) c)]

-- | Start a new open path at the given point.
moveTo :: v -> PathContext v -> PathContext v
moveTo v = modifyCurrent v . modifyStart v . newPath

-- | Start a new open ('False') or closed ('True') path at the given point.  
moveTo' :: v -> Bool -> PathContext v -> PathContext v
moveTo' v b = modifyClosed b . modifyCurrent v . modifyStart v . newPath

-- | Add a line segment to the current path.
lineTo :: Num v => v -> PathContext v -> PathContext v
lineTo v p = appendSegment (straight v) p

-- | Add a quadratic bezier segment to the current path.
quadTo :: Fractional v => v -> v -> PathContext v -> PathContext v
quadTo a b p = appendSegment (bezier2 a b) p

-- | Add a cubic bezier segment to the current path.
curveTo :: Num v => v -> v -> v -> PathContext v -> PathContext v
curveTo a b c p = appendSegment (bezier3 a b c) p

-- | <http://en.wikipedia.org/wiki/B%C3%A9zier_curve#Degree_elevation>
bezier2 :: Fractional v => v -> v -> Segment v
bezier2 a b = bezier3 (2*a/3) ((2*a+b)/3) b
