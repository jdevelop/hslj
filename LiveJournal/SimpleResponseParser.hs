module LiveJournal.SimpleResponseParser where

import LiveJournal.Pair
import Data.Map as M 

type IxPair = (Int, String, String)

type IxPairParser = Pair -> Maybe IxPair

type RecordUpdateF r = String -> String -> r -> r

convertPairsToRecords :: RecordUpdateF r -> IxPairParser -> r -> [Pair] -> [r]
convertPairsToRecords updateRecF parsePairF initRec pairs = M.elems $ foldl ( processWithPairs ) initMap pairs
    where
        initMap = M.empty
        processWithPairs resMap = maybe resMap (updateMapF resMap) . parsePairF
        updateMapF res (idx, key, value) | idx `M.member` res = M.adjust (updateRecF key value) idx res
                                         | otherwise = M.insert idx (updateRecF key value initRec) res
