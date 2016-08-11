module Circuit
    ( Circuit(..)
    , Gate(..)
    , Control(..)
    , getCtrlLn
    , gateCount
    , countToff
    , countTGates
    ) where

import Data.Text (Text)

import Data.Vector (Vector)
import qualified Data.Vector as V

data Control = Neg Int | Pos Int
    deriving (Show)

data Circuit =
    Circuit { circLines :: Vector Text
            , circGates :: Vector Gate }
    deriving (Show)

data Gate =   Toff (Vector Control) Int
            | Fred (Vector Control) Int Int
            | OneBit Text Int --Name and target
    deriving (Show)

countToff :: Circuit -> Int
countToff circ = V.sum . V.map toffCost $ gs
    where gs = circGates circ
          toffCost (Toff ctrls _) = max 0 $ (length ctrls-1)*2-1
          toffCost _ = 0

countTGates :: Circuit -> Int
countTGates circ = 7 * countToff circ + (V.sum . V.map tCost $ gs)
    where gs = circGates circ
          tCost (OneBit "T" _) = 1
          tCost _ = 0

getCtrlLn :: Control -> Int
getCtrlLn (Neg n) = n
getCtrlLn (Pos n) = n

gateCount :: Circuit -> Int
gateCount = length . circGates
