module CircuitDiagram (circuitToSvg) where

import Prelude
import Diagrams.Prelude hiding ((<>))
import Diagrams.Backend.SVG
import Graphics.SVGFonts

import qualified Debug.Trace as D

import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as V

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

drawGate :: Gate -> Diagram B
drawGate gate@(Toff cs t) =
                 circle targetRad # lwL lWidth # translateY (fromIntegral t)
              <> controls
              <> line
  where line = drawLine 0 (top - bottom)
             # translateY bottom
          where top | maxY == t = fromIntegral maxY + targetRad
                    | otherwise = fromIntegral maxY
                bottom | minY == t = fromIntegral minY - targetRad
                       | otherwise = fromIntegral minY
                ctrlLns = fmap (fromIntegral . getCtrlLn) cs
                (maxY,minY) = gateToRange gate
        controls = mconcat . fmap drawCtrl . V.toList $ cs
drawGate (OneBit nm t) = (symb <> base) # translateY (fromIntegral t)
  where symb = drawChar $ T.head nm
        base = square (1.7*targetRad)
             # lw thin
             # fc white

drawChar :: Char -> Diagram B
drawChar c = strokeP (textSVG' (TextOpts bit INSIDE_H KERN False charSize charSize) [c])
             # lw 0.0
             # fc black


drawCtrl :: Control -> Diagram B
drawCtrl (Neg y) = circle ctrlRad # fc white # translateY (fromIntegral y)
drawCtrl (Pos y) = circle ctrlRad # fc black # translateY (fromIntegral y)

drawSwap :: Double -> Double -> Vector Control -> Diagram B
drawSwap t1 t2 cs = targ # translateY t1
                 <> targ # translateY t2
                 <> controls
                 <> line
  where targ =     drawLine 1 1  # center
                <> drawLine 1 (-1) # center
        line = drawLine 0 (top - bottom)
             # translateY bottom
          where ctrlLns = fmap (fromIntegral . getCtrlLn) cs
                top = max (max t1 t2) (V.maximum ctrlLns)
                bottom = min (min t1 t2) (V.minimum ctrlLns)
        controls = mconcat . fmap drawCtrl . V.toList $ cs

drawLine :: Double -> Double -> Diagram B
drawLine x y = fromSegments [straight $ r2(x,y) ] # lwL lWidth

gateToRange :: Gate -> (Int,Int)
gateToRange g =
  case g of
    Toff ctrls t -> if null ctrls then
                      (t,t)
                    else
                      let ctrlLns = fmap getCtrlLn ctrls
                      in (max t $ V.maximum ctrlLns, min t $ V.minimum ctrlLns)
    OneBit _ t -> (t,t)

-- | Groups a list of gates such that each group can be drawn without
-- overlap in the same column
getDrawCols :: Vector Gate -> [[Gate]]
getDrawCols = (\(x,y) -> x ++ [y]) . V.foldl' colFold ([[]],[])
  where colFold (res,curr) next =
          if fits (fmap gateToRange curr) (gateToRange next)
          then (res, next : curr)
          else (res ++ [curr], [next])
        fits [] _ = True
        fits rs r = all (notInRange r) rs
          where notInRange x y = minT x > maxT y || maxT x < minT y
                maxT = uncurry max
                minT = uncurry min
