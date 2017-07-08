module Main where

import Control.Monad  (join)
import Data.Bifunctor (bimap)
import Data.Bits
import qualified Data.PQueue.Max as PQ

type R = Double
type Interval = (R, R)
type Rec = [Interval]
type RecSet = [Rec]
type OrderedRecSet = PQ.MaxQueue (R, Rec)
type Oracle = [R] -> Bool

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
boundingBox o r = zip bot top'
    where (bot, top) = (fst <$> r, snd <$> r)
          corner i = zipWith (curry $ convexComb 1) top (basis i)
          basis i = [if j == i then 1 else 0 | j <- [1..]]
          f i = (snd <$> binsearchRec o (zip bot $ corner i)) !! i
          top' = f <$> [1..]


convexComb :: Num a => a -> (a,a) -> a
convexComb a (x,y) = x + (y-x)*a

refineRec :: Oracle -> Rec -> RecSet
refineRec o r = subdivide (binsearchRec o r) r

binsearchRec :: Oracle -> Rec -> Rec
binsearchRec o r = fInv $ binsearch $ o . f
    where f a = convexComb a <$> r
          fInv = uncurry zip . join bimap f

eps = 0.1

-- TODO: implement edge cases
binsearch :: (R -> Bool) -> Interval
binsearch o = binsearch' o (0,1)

binsearch' o i@(lo,hi) | hi - lo <= eps = i
                       | o m = binsearch' o (lo, m)
                       | otherwise = binsearch' o (m, hi)
    where m = convexComb 0.5 i

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
          q'' = q' `PQ.union` PQ.fromList (zip (volume <$> refined) refined)
