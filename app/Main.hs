module Main where

import Lib
import ParseQC
import Circuit
import CircuitDiagram

import Prelude hiding (getContents)

import Data.Text.IO (getContents)
import Data.Text (Text)

import Data.Aeson
import Data.Aeson.TH

import qualified Data.ByteString.Lazy.Char8 as BS

import System.IO (hPutStrLn,stderr)

main :: IO ()
main = do
    qc <- parseQC "stdin" <$> getContents
    case qc of
      Left x ->
        hPutStrLn stderr $ show x
      Right qc' -> do
          let circ = qcToCirc qc'
          case circ of
              Left x ->
                  hPutStrLn stderr $ show x
              Right circ' ->
                  writeCircInfo circ'

writeCircInfo circ = do
    let circInfo = CircuitInfo {
             numGates = gateCount circ
           , numBits = (length . circLines) circ
           , numToff = 0
           , numT = 0
           , circuitFile = "circ.svg"
          }
    BS.putStrLn . encode $ circInfo
    circuitToSvg circ "circ.svg" 1000

data CircuitInfo = CircuitInfo {
      numGates :: Int
    , numBits :: Int
    , numToff :: Int
    , numT :: Int
    , circuitFile :: String
}
$(deriveJSON defaultOptions ''CircuitInfo)
