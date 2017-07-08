{-# LANGUAGE ScopedTypeVariables #-}
module TimeSeries where

import Control.Arrow (first)
import Data.IntervalMap
import Data.IntervalMap.Interval

-- Time Series
type TimeSeries b = [(Double, b)]
type TimeSeries' b = IntervalMap Double b

oo = 1/0

toIntervalList :: [(Double, b)] -> [(Interval Double, b)]
toIntervalList xs = zip intervals (snd <$> xs)
    where
      times = (fst <$> xs) ++ [oo]
      intervals = zipWith IntervalCO times (tail times)
                  
toTree :: TimeSeries b -> TimeSeries' b
toTree = fromList . toIntervalList

fromTree :: TimeSeries' b -> TimeSeries b
fromTree = fmap (first lowerBound) . toAscList

compress :: TimeSeries' b -> TimeSeries' b
compress = flattenWith const

tsBinOp :: (a -> a -> a) -> TimeSeries a -> TimeSeries a -> TimeSeries a
tsBinOp op a b = fromTree $ tsBinOp' op (toTree a) (toTree b)

tsBinOp' :: (a -> a -> a) -> TimeSeries' a -> TimeSeries' a -> TimeSeries' a
tsBinOp' op a b = compress $ unionWith op a b



