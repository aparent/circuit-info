module ParseQC
    ( parseQC
    ,  QC(..)
    , qcToCirc
    ) where

import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Char as C
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.List as L


import Text.Parsec
import Text.Parsec.Text

import Circuit

data ConversionError =  NoVars
                     | UnknownLine Text
    deriving (Show)

qcToCirc :: QC -> Either ConversionError Circuit
qcToCirc (QC infoLns gs) = do
    lines' <- lineNames
    gs' <- gateVals lines'
    return Circuit { circLines = V.fromList lines', circGates = V.fromList gs'}
    where lineNames =
            case findInfoLine "v" infoLns of
                Just x -> Right $ infoLineVals x
                Nothing -> Left NoVars
            where infoLineName (InfoLine n _ ) = n
                  infoLineVals (InfoLine _ vs ) = vs
                  findInfoLine nm = L.find ( (== nm) . infoLineName )
          gateVals lns = mapM toGate gs
            where indexMap = M.fromList (zip lns [0..])
                  toGate (ParsedGate name lineDeps) = do
                     targ <- lookupLineNumber $ last lineDeps
                     ctrls <- mapM toControls  $ init lineDeps
                     case () of
                       _ | isToff name -> return $ Toff (V.fromList $ ctrls) targ
                         | otherwise -> return $ OneBit name targ
                    where lookupLineNumber nm =
                            case M.lookup nm indexMap of
                                Just n -> Right n
                                Nothing -> Left (UnknownLine nm)
                          toControls nm | T.last nm == '\'' = Neg <$> (lookupLineNumber $ T.init nm)
                                        | otherwise = Pos <$> lookupLineNumber nm
                  isToff name = (T.length name > 1 && C.isDigit (T.index name 1))
                             || T.toLower name == "tof"
                             || T.toLower name == "toff"
                             || T.toLower name == "not"
                             || T.toLower name == "cnot"
                    where lname = T.toLower name


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
    _ <- char '#'
    _ <- manyTill anyChar endOfLine
    spaces
    return ()

ident :: Parser Text
ident = do
   id <- many1 (alphaNum <|> char '\'') <* many (char ' ')
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
