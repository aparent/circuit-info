module ParseQC
    ( parseQC
    ,  QC(..)
    , qcToCirc
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import qualified Data.List as L


import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Text

import Circuit

data ConversionError =  NoVars
                     | UnknownLine Text
    deriving (Show)

qcToCirc :: QC -> Either ConversionError Circuit
qcToCirc (QC infoLns gs) = do
    lines' <- lineNames
    gs' <- gateVals lines'
    return $ Circuit { circLines = V.fromList lines', circGates = V.fromList gs'}
    where lineNames =
            case (findInfoLine "v" $ infoLns) of
                Just x -> Right $ infoLineVals x
                Nothing -> Left NoVars
            where infoLineName (InfoLine n _ ) = n
                  infoLineVals (InfoLine _ vs ) = vs
                  findInfoLine nm = L.find ( (== nm) . infoLineName )
          gateVals lns = mapM toGate gs
            where indexMap = M.fromList (zip lns [0..])
                  toGate (ParsedGate name lineDeps) = do
                     ld <- mapM lookupLineNumber . reverse $ lineDeps
                     case () of
                       _ |    T.toLower name == "tof"
                           || T.toLower name == "toff"
                           || T.toLower name == "cnot" ->
                            return $ Toff (V.fromList $ tail ld) (head ld)
                         | otherwise -> return $ OneBit name (head ld)
                    where lookupLineNumber nm =
                            case (M.lookup nm indexMap) of
                                Just n -> Right n
                                Nothing -> Left (UnknownLine nm)


data QC = QC [InfoLine] [ParsedGate]
    deriving (Show)

data InfoLine = InfoLine Text [Text]
    deriving (Show)

data ParsedGate = ParsedGate Text [Text]
    deriving (Show)

parseQC :: String -> Text -> Either ParseError QC
parseQC = parse qc

qc :: Parser QC
qc = QC <$> info <*> gates

comment :: Parser ()
comment = do
    char '#'
    manyTill anyChar endOfLine
    spaces
    return ()

ident :: Parser Text
ident = do
   id <- many1 alphaNum <* many (char ' ')
   return (T.pack id)

info :: Parser [InfoLine]
info = many line
    where line = do
            name <- char '.' *> ident
            vals <- many ident <* spaces
            return $ InfoLine name vals

gates :: Parser [ParsedGate]
gates =    (string "begin" <|> string "BEGIN")
        *> spaces
        *> manyTill gs (try $ string "end" <|> string "END")
    where gs = ParsedGate <$> ident <*> many1 ident <* spaces
