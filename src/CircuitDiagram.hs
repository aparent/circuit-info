module CircuitDiagram (circuitToSvg) where

import Prelude
import Diagrams.Prelude hiding ((<>))
import Diagrams.Backend.SVG
import Graphics.SVGFonts

import Data.List (foldl')

import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Text (Text)
import qualified Data.Text as T

import Circuit

lWidth, targetRad, charSize, ctrlRad, colSpace :: Double
lWidth = 0.07
targetRad = 0.4
charSize = 1
ctrlRad = 0.15
colSpace = 0.3

-- | Takes a circuit, filename, and width then writes out a
-- diagram of the circuit as an SVG
circuitToSvg :: Circuit -> String -> Double -> IO ()
circuitToSvg c f w = renderSVG f (mkWidth w) $ drawCirc c

drawCirc :: Circuit -> Diagram B
drawCirc c = hsep 0.0 [ txt
                      , gs # centerX <> ls
                      , txt
                      ] # frame 1
  where gs = drawGates (circGates c)
        ls = mconcat . fmap (mkLine.fromIntegral) $ [0.. (length.circLines) c -1 ]
          where mkLine y = ( hrule . width ) gs
                         # lwL lWidth
                         # lc black
                         # translateY y
        txt = mconcat . zipWith placeText (V.toList $ circLines c) $ fmap fromIntegral [0..(length.circLines) c -1 ]
          where placeText s y = (mkText s <> phantom (rect 4 1 :: D V2 Double))
                              # translateY y
                mkText s = text (T.unpack s)
                         # fontSize (local 0.5)
        drawGates = hsep colSpace . map (mconcat . map drawGate) . getDrawCols
        drawGate g =
          case g of
            Toff ctrls t -> drawCnot (fromIntegral t) $ fmap fromIntegral ctrls
            OneBit nm t -> drawOneBit (T.head $ T.toUpper nm) $ fromIntegral t

drawChar :: Char -> Diagram B
drawChar c = strokeP (textSVG' (TextOpts bit INSIDE_H KERN False charSize charSize) [c])
             # lw 0.0
             # fc black

drawOneBit :: Char -> Double -> Diagram B
drawOneBit ch t = (symb <> base)  # translateY t
  where symb = drawChar ch
        base = square (1.7*targetRad)
             # lw thin
             # fc white


drawCnot :: Double -> Vector Double -> Diagram B
drawCnot t cs =  circle targetRad # lwL lWidth # translateY t
              <> controls
              <> line
  where line = drawLine 0 (top - bottom)
             # translateY bottom
          where top | maxY == t = maxY + targetRad
                    | otherwise = maxY
                bottom | minY == t = minY - targetRad
                       | otherwise = minY
                maxY = max t $ maximum cs
                minY = min t $ minimum cs
        controls = mconcat . V.toList . fmap drawCtrl $ cs

drawCtrl :: Double -> Diagram B
drawCtrl y = circle ctrlRad # fc black # translateY y

drawSwap :: Double -> Double -> Vector Double -> Diagram B
drawSwap t1 t2 cs = targ # translateY t1
                 <> targ # translateY t2
                 <> controls
                 <> line
  where targ =     drawLine 1 1  # center
                <> drawLine 1 (-1) # center
        line = drawLine 0 (top - bottom)
             # translateY bottom
          where top = max (max t1 t2) (V.maximum cs)
                bottom = min (min t1 t2) (V.minimum cs)
        controls = mconcat . V.toList . fmap drawCtrl $ cs

drawLine :: Double -> Double -> Diagram B
drawLine x y = fromSegments [straight $ r2(x,y) ] # lwL lWidth

-- | Groups a list of gates such that each group can be drawn without
-- overlap in the same column
getDrawCols :: Vector Gate -> [[Gate]]
getDrawCols = (\(x,y) -> x ++ [y]) . V.foldl' colFold ([[]],[])
  where colFold (res,curr) next =
          if fits (fmap gateToRange curr) (gateToRange next)
          then (res, next : curr)
          else (res ++ [curr], [next])
        gateToRange :: Gate -> (Int,Int)
        gateToRange g =
          case g of
            Toff ctrls t -> ((max t $ V.maximum ctrls), (min t $ V.minimum ctrls))
            OneBit _ t -> (t,t)
        fits [] _ = True
        fits rs r = all (notInRange r) rs
          where notInRange x y = minT x > maxT y || maxT x < minT y
                maxT = uncurry max
                minT = uncurry min
