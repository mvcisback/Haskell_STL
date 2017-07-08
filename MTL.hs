module MTL where

import Prelude hiding (lookup)
import Data.Map hiding (filter)
import qualified Data.IntervalMap as IM
import Data.Maybe

import Algebra.Lattice

import TimeSeries (TimeSeries, tsBinOp)

type Interval = (Double, Double)

data MTL = AtomicPred String
     | Neg MTL
     | Top
     | Bot
     | And MTL MTL
     | Or MTL MTL
     | Implies MTL MTL
     | IFF MTL MTL
     | XOR MTL MTL
     | U MTL MTL
     | F Interval MTL
     | G Interval MTL
     | TimedU Interval MTL MTL

instance JoinSemiLattice MTL where
    e \/ e' = Or e e'

instance MeetSemiLattice MTL where
    e /\ e' = And e e'


type TS = TimeSeries Bool
type Store = Map String TS


implies :: Bool -> Bool -> Bool
implies a b = (not a) || b

iff :: Bool -> Bool -> Bool
iff a b = (a `implies` b) && (b `implies` a)

xor :: Bool -> Bool -> Bool
xor a b = (a || b) && (not a || not b)

eval :: MTL -> Store -> TS
eval (AtomicPred p) s = fromMaybe [(0, False)] $ lookup p s

eval Top _ = [(0, True)]
eval Bot _ = [(0, False)]

eval (Neg e) s =  fmap not <$> eval e s

eval (e `Or` e') s  = tsBinOp (||) (eval e s) (eval e' s) 
eval (e `And` e') s  = tsBinOp (&&) (eval e s) (eval e' s)
eval (e `Implies` e') s = tsBinOp implies (eval e s) (eval e' s)
eval (e `IFF` e') s = tsBinOp iff (eval e s) (eval e' s)
eval (e `XOR` e') s = tsBinOp xor (eval e s) (eval e' s)

eval (F i e) s = evalF i $ eval e s
eval (G i e) s = evalG i $ eval e s
eval (TimedU i@(a, b) e e') s = eval e'' s
    where e'' = (F i e') `And` (G (0, a) (e `U` e'))
eval (e `U` e') s = undefined

evalG (a, b) = fmap $ \(t, v) -> (max 0 (if v then t-a else t-b), v)
evalF (a, b) = fmap $ \(t, v) -> (max 0 (if v then t-b else t-a), v)
