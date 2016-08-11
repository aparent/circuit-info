module Main where

import ParseQC
import Circuit
import CircuitDiagram

import Prelude hiding (getContents)

import Data.Text.IO (getContents)

import Data.Aeson
import Data.Aeson.TH

import qualified Data.ByteString.Lazy.Char8 as BS

import System.IO (hPrint,stderr)

import Options.Applicative

main :: IO ()
main = do
    progOpts <- execParser opts
    qc <- parseQC "stdin" <$> getContents
    case qc of
      Left x ->
        hPrint stderr x
      Right qc' -> do
          let circ = qcToCirc qc'
          case circ of
              Left x ->
                  hPrint stderr x
              Right circ' ->
                  writeCircInfo circ' (imagePath progOpts)

data ProgOptions = ProgOptions
  { imagePath :: String }

opts :: ParserInfo ProgOptions
opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Reads a .qc formatted circuit from stdin.\
                 \ Outputs information about the circuit as JSON.\
                 \ Also generates and ouputs an image of the circuit."
     <> header "Circuit Info" )
    where options :: Parser ProgOptions
          options = ProgOptions
              <$> strOption
              ( short 'i'
              <> value "circ.svg"
              <> showDefault
              <> metavar "PATH"
              <> help "Output svg image to PATH" )

writeCircInfo :: Circuit -> String -> IO ()
writeCircInfo circ imgPath = do
    let circInfo = CircuitInfo {
             numGates = gateCount circ
           , numBits = (length . circLines) circ
           , numToff = countToff circ
           , numT = countTGates circ
           , circuitFile = imgPath
          }
    BS.putStrLn . encode $ circInfo
    circuitToSvg circ imgPath 1000

data CircuitInfo = CircuitInfo {
      numGates :: Int
    , numBits :: Int
    , numToff :: Int
    , numT :: Int
    , circuitFile :: String
}
$(deriveJSON defaultOptions ''CircuitInfo)
