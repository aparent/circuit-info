module Circuit
    ( Circuit(..)
    , Gate(..)
    , Control(..)
    , getCtrlLn
    , gateCount
    ) where

import Data.Text (Text)

import Data.Vector (Vector)

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


getCtrlLn :: Control -> Int
getCtrlLn (Neg n) = n
getCtrlLn (Pos n) = n

gateCount :: Circuit -> Int
gateCount = length . circGates
