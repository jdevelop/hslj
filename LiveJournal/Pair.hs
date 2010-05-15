module LiveJournal.Pair where

import Data.ByteString.UTF8 as BStr
import Data.Maybe
import Prelude as P

data Pair = Pair { name, value :: BStr.ByteString }

makePair :: String -> String -> Pair
makePair strName strValue = Pair strName' strValue'
    where
        strName' = BStr.fromString strName
        strValue' = BStr.fromString strValue

makePairBSName :: BStr.ByteString -> String -> Pair
makePairBSName strName strValue = Pair strName strValue'
    where
        strValue' = BStr.fromString strValue

makePairBSValue :: String -> BStr.ByteString -> Pair
makePairBSValue strName strValue = Pair strName' strValue
    where
        strName' = BStr.fromString strName

findPair :: String -> [Pair] -> Maybe BStr.ByteString
findPair pName = listToMaybe . P.map value . P.filter ( ( == pName') . name )
    where
        pName' = BStr.fromString pName

instance Show Pair where
    show (Pair name' value') = BStr.toString name' ++ "=" ++ BStr.toString value'
