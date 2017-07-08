{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Maybe
import Data.Bits
import Numeric.LinearAlgebra
import qualified Data.PQueue.Max as PQ

type Interval = (R, R)
type Rec = [Interval]
type RecSet = [Rec]
type OrderedRecSet = PQ.MaxQueue (R, Rec)
type Oracle = Vector R -> Bool


subdivide :: Rec -> Rec -> RecSet
subdivide tol r = alphaToRec <$> alphas
    where l = length r
          upwardCone = zipWith (\(lo, _) (_, top) -> (lo, top))
          downwardCone = zipWith (\(_, hi) (bot, _) -> (bot, hi))
          wordToList w = testBit w <$> [0..l-1]
          alphaToRec a = zipWith3 pick a (upwardCone tol r) (downwardCone tol r)
          alphas = wordToList <$> ([1..2^l-2] :: [Word])
          pick flag a b = if flag then a else b


boundingBox :: Oracle -> Rec -> Rec
boundingBox = undefined

refineRec :: Oracle -> Rec -> RecSet
refineRec o r = refineRec' (binsearch o r) r

binsearch :: Oracle -> Rec -> Vector R
binsearch = undefined

refineRec' :: Vector R -> Rec -> RecSet
refineRec' = undefined

-- Volume Guided Refinement

volume :: Rec -> R
volume = product . fmap (abs . uncurry (-))

volumeGuidedRefinement :: Oracle -> Rec -> [OrderedRecSet]
volumeGuidedRefinement o r = vgr o $ PQ.singleton (volume r', r')
    where r' = boundingBox o r

vgr :: Oracle -> OrderedRecSet -> [OrderedRecSet]
vgr o q = q'' : vgr o q''
    where Just ((_, r), q') = PQ.maxView q
          refined = refineRec o r
          q'' = q' `PQ.union` (PQ.fromList $ zip (volume <$> refined) refined)
