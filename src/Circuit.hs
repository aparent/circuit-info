module Circuit
    ( Circuit(..)
    , Gate(..)
    , gateCount
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Vector (Vector)
import qualified Data.Vector as V

data Circuit =
    Circuit { circLines :: Vector Text
            , circGates :: Vector Gate }
    deriving (Show)

data Gate =   Toff (Vector Int) Int
            | Fred (Vector Int) Int Int
            | OneBit Text Int --Name and target
    deriving (Show)

gateCount :: Circuit -> Int
gateCount = length . circGates