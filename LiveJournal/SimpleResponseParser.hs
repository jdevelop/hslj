module LiveJournal.SimpleResponseParser (
    IxPair,
    IxPairParser,
    RecordUpdateF,
    simpleLJResponseNameParser,
    convertPairsToRecords
) where

import LiveJournal.Pair
import Data.Map as M 
import Text.ParserCombinators.ReadP as TPR
import Data.Char as C
import Data.List as L

type IxPair = (Int, String, String)

type IxPairParser = Pair -> Maybe IxPair

type RecordUpdateF r = String -> String -> r -> r

convertPairsToRecords :: RecordUpdateF r -> IxPairParser -> r -> [Pair] -> [r]
convertPairsToRecords updateRecF parsePairF initRec pairs = M.elems $ L.foldl' processWithPairs initMap pairs
    where
        initMap = M.empty
        processWithPairs resMap = maybe resMap (updateMapF resMap) . parsePairF
        updateMapF res (idx, key, value) | idx `M.member` res = M.adjust (updateRecF key value) idx res
                                         | otherwise = M.insert idx (updateRecF key value initRec) res

simpleLJResponseNameParser :: String -> ReadP (Int, String)
simpleLJResponseNameParser prefix = do
    TPR.string prefix
    paramId <- TPR.munch C.isDigit
    TPR.char '_'
    paramName <- TPR.munch (\_ -> True)
    return (read paramId,paramName)
